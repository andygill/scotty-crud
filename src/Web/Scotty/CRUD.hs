{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Web.Scotty.CRUD where

import Web.Scotty
import Data.Aeson
import Data.Aeson.Parser as P
import Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Control.Applicative
import Data.Char (isSpace, isDigit, chr)
import Data.List (foldl')
import Data.Text (Text, pack)
import Control.Monad
import qualified Data.Text as Text
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import System.IO

------------------------------------------------------------------------------------
-- Basic synonyms for key structures 
--
type Id        = Text
type Table row = HashMap Id row
type Row       = Object

------------------------------------------------------------------------------------
-- Table
readTable :: CRUDRow row => Handle -> IO (Table row)
readTable h = do

    let sz = 32 * 1024 :: Int

    let loadCRUD bs env
          | BS.null bs = do
                  bs' <- BS.hGet h sz
                  if BS.null bs'
                  then return env        -- done, done, done (EOF)
                  else loadCRUD bs' env
          | otherwise =
                  parseCRUD (Atto.parse P.json bs) env
        parseCRUD (Fail bs _ msg) env
                | BS.all (isSpace . chr . fromIntegral) bs = loadCRUD BS.empty env
                | otherwise = fail $ "parse error: " ++ msg
        parseCRUD (Partial k) env = do
                  bs <- BS.hGet h sz    
                  parseCRUD (k bs) env
        parseCRUD (Done bs r) env = do
                  case fromJSON r of
                    Error msg -> error msg
                    Success update -> loadCRUD bs $! tableUpdate update env

    loadCRUD BS.empty HashMap.empty 


writeTableUpdate :: CRUDRow row => Handle -> TableUpdate row -> IO ()
writeTableUpdate h row = do
        LBS.hPutStr h (encode row)
        LBS.hPutStr h "\n" -- just for prettyness, nothing else
                     
writeTable :: CRUDRow row => Handle -> Table row -> IO ()
writeTable h table = sequence_
        [ writeTableUpdate h $ RowUpdate (Named iD row)
        | (iD,row) <- HashMap.toList table
        ]

------------------------------------------------------------------------------------
-- CRUD

-- | A CRUD is a OO-style database Table, with getters and setters, a table of typed rows.
data CRUD m row = CRUD
     { createRow :: row       -> m (Named row)
     , getRow    :: Id        -> m (Maybe (Named row))
     , getTable               :: m (Table row)
     , updateRow :: Named row -> m ()
     , deleteRow :: Id        -> m () -- alway works
     , sync                   :: m ()
     }

-- | take a STM-based CRUD, and return a IO-based CRUD
atomicCRUD :: CRUD STM row -> CRUD IO row
atomicCRUD crud = CRUD 
     { createRow = atomically . createRow crud
     , getRow    = atomically . getRow crud
     , getTable  = atomically $ getTable crud
     , updateRow = atomically . updateRow crud 
     , deleteRow = atomically . deleteRow crud
     , sync      = atomically $ sync crud
     }

-- | We store our CRUD in a simple format; a list of newline seperated
-- JSON objects, in the order they were applied, where later objects
-- subsumes earlier ones. If the Handle provided is ReadWrite,
-- the subsuquent updates are recorded after the initial ones.
-- There is no attempt a compaction; we only append to the file.
-- 
-- Be careful: the default overloading of () for FromJSON
-- will never work, because ...

readCRUD :: forall row . (Show row, CRUDRow row) => Handle -> IO (CRUD STM row)
readCRUD h = do

    env <- readTable h 

--    print env

    -- This is our table,
    table <- newTVarIO env
    updateChan <- newTChanIO
    
    let top :: STM Integer
        top = do t <- readTVar table
                 return $ foldr max 0
                          [ read (Text.unpack k)
                          | k <- HashMap.keys t
                          , Text.all isDigit k
                          ]

    uniq <- atomically $ do
               mx <- top
               newTVar (mx + 1)

    -- Get the next, uniq id when creating a row in the table.
    let next :: STM Text           
        next = do
               n <- readTVar uniq
               let iD = Text.pack (show n) :: Text
               t <- readTVar table
               if HashMap.member iD t
               then do mx <- top
                       t <- writeTVar uniq (mx + 1)
                       next
                 -- Great, we can use this value
               else do writeTVar uniq $! (n + 1)
                       return iD

    let updateCRUD update = do
          modifyTVar table (tableUpdate update)
          writeTChan updateChan update

    let handler m = m `catches`
         []
{-
          [ {-Handler $ \ (ex :: SomeAsyncException) -> return ()
          , -}Handler $ \ (ex :: SomeException) -> do { print ("X",ex) ; return (); } 
                          -- print ("XX",ex) ; return () }
          ]
-}
    flushed <- newTVarIO True
    forkIO $ handler $ forever $ do
          tu <- atomically $ do
                  writeTVar flushed False
                  readTChan updateChan
--          print $ "writing" ++ show tu
          LBS.hPutStr h (encode tu)
          LBS.hPutStr h "\n" -- just for prettyness, nothing else
          hFlush h
          atomically $ writeTVar flushed True
          return ()

    let syncCRUD = do
            flush_status <- readTVar flushed
            chan_status <- isEmptyTChan updateChan
            check (flush_status && chan_status)

    return $ CRUD
     { createRow = \ row    -> do iD <- next
                                  let row' = Named iD row
                                  updateCRUD (RowUpdate row')
                                  return row'
     , getRow    = \ iD     -> do t <- readTVar table
                                  return $ fmap (Named iD) $ HashMap.lookup iD t
     , getTable  =             do readTVar table
     , updateRow = updateCRUD . RowUpdate 
     , deleteRow = updateCRUD . RowDelete
     , sync = syncCRUD
     }

test = do
        h <- openFile "test.json" ReadWriteMode 
        crud :: CRUD STM Object <- readCRUD h
        v <- atomically $ createRow crud $ HashMap.fromList [("XX",String "32345234")]
        print v
        return ()



--    return (error "")

-- create a CRUD from a HashMap. If you want to extract the CRUD,
-- use getTable (which gives the updated HashMap).

--        (RESTfulWRITE -> IO ()) -> 


{-
-- ToDo:

createCRUD :: (FromJSON row, ToJSON row) => HashMap Text row -> IO (CRUD STM row)
createCRUD = error ""

datatypeCRUD :: forall m row . (Monad m, CRUDRow row) => CRUD m row -> CRUD m Object
datatypeCRUD crud = CRUD {}
     { createRow = return . toObject <=< createRow crud <=< fromObject
     , getRow    = \ iD -> do optRow <- getRow crud iD
                              case optRow of
                                Nothing -> return Nothing
                                Just row -> liftM Just $ return $ toObject row
     , getTable = liftM (fmap toObject) $ getTable crud
     , updateRow = \ iD -> updateRow crud iD <=< fromObject
     , deleteRow = deleteRow crud
     , sync      = sync crud
     }
 
 where toObject :: NamedRow row -> Object
       toObject r = case toJSON r of
                  Object obj -> obj
                  _ -> error "row is not representable as an Object"
       fromObject :: Object -> m row
       fromObject obj = case fromJSON (Object obj) of
                       Success r -> return r
                       Error err -> fail err
-}

{-
-- | create a CRUD that does not honor write requests.
readOnlyCRUD :: (Monad m) => CRUD m row -> CRUD m row
readOnlyCRUD crud = CRUD 
     { createRow = \ iD  -> fail ""
     , getRow    = \ iD     -> getRow crud iD
     , getTable  =             getTable crud
     , updateRow = \ iD row -> fail ""
     , deleteRow = \ iD     -> fail ""
     , sync      = sync crud
     }
-}
------------------------------------------------------------------------------------
-- | crud takes a directory path, and a URL, and returns
-- a scotty monad that provides a RESTful CRUD for this URL collection.

scottyCRUD :: (FromJSON row, ToJSON row) => FilePath -> CRUD IO row -> ScottyM ()
scottyCRUD dir url = do return ()
{-        
        get (capture url) $ do 
                return ()
        
        get (capture $ url ++ "/:id") $ do return ()
-}                
                
----------------------------------------------------------------------

-- Changes all all either an update (create a new field if needed) or a delete.

data TableUpdate row
        = RowUpdate (Named row)
        | RowDelete Id
        deriving (Show, Eq)
        
{-
instance FromJSON RESTfulWRITE where
    parseJSON (Object v) = 
        (do Object obj <- case HashMap.lookup "update" v of
                     Nothing -> fail "no update"
                     Just v -> return v
            flip UpdateModel obj <$> (obj .: "id")
        ) <|> 
        ( DeleteModel <$>
               v .: "delete"
        )
-}
        
instance ToJSON row => ToJSON (TableUpdate row) where
   -- Assumption: the obj contains an "id" key
   toJSON (RowUpdate namedRow) = toJSON namedRow
   toJSON (RowDelete key)      = Object $ HashMap.fromList [("delete",String key)]

instance FromJSON row => FromJSON (TableUpdate row) where
    parseJSON (Object v) = 
        ( RowUpdate <$> parseJSON (Object v)
        ) <|> 
        ( RowDelete <$>
               v .: "delete"
        )

tableUpdate :: TableUpdate row -> HashMap Text row -> HashMap Text row
tableUpdate (RowUpdate (Named key row)) = HashMap.insert key row
tableUpdate (RowDelete key)             = HashMap.delete key

----------------------------------------------------

-- It must be the case that toJSON never fails for any row (toJSON is total)
-- and toJSON for a row must always returns an object.

class (ToJSON row, FromJSON row) => CRUDRow row where
   lensID :: (Functor f) => (Text -> f Text) -> row -> f row

-- (CRUDRow row) => toJSON row /= _|_
-- (CRUDRow row) => toJSON row == Object {...}


instance CRUDRow Object where
   lensID f m = (\ v' -> HashMap.insert "id" (String v') m) <$> f v
       where v = case HashMap.lookup "id" m of
                   Just (String v) ->  v
                   _ -> "" -- by choice

----------------------------------------------------
data Named row = Named Id row
   deriving (Eq,Show)

instance FromJSON row => FromJSON (Named row) where
    parseJSON (Object v) = Named
                <$> v .: "id"
                <*> (parseJSON $ Object $ HashMap.delete "id" v)
                
instance ToJSON row => ToJSON (Named row) where                
   toJSON (Named key row) = 
                   case toJSON row of
                     Object env -> Object $ HashMap.insert "id" (String key) env
                     _ -> error "row should be an object"
        