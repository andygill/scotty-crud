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
import System.IO

------------------------------------------------------------------------------------

-- | A CRUD is a OO-style database Table, with getters and setters, a table of typed rows.
data CRUD m row = CRUD
     { createRow :: row         -> m row
     , getRow    :: Text        -> m (Maybe row)
     , getTable                 :: m (HashMap Text row)
     , updateRow :: Text -> row -> m ()
     , deleteRow :: Text        -> m () -- alway works
     }

-- | take a STM-based CRUD, and return a IO-based CRUD
atomicCRUD :: CRUD STM row -> CRUD IO row
atomicCRUD crud = CRUD 
     { createRow = \ row    -> atomically $ createRow crud row
     , getRow    = \ iD     -> atomically $ getRow crud iD
     , getTable  =             atomically $ getTable crud
     , updateRow = \ iD row -> atomically $ updateRow crud iD row
     , deleteRow = \ iD     -> atomically $ deleteRow crud iD
     }

-- | We store our CRUD in a simple format; a list of newline seperated
-- JSON objects, in the order they were applied, where later objects
-- subsumes earlier ones. If the Handle provided is ReadWrite,
-- the subsuquent updates are recorded after the initial ones.
-- There is no attempt a compaction; we only append to the file.
-- 
-- Be careful: the default overloading of () for FromJSON
-- will never work, because 

readCRUD :: forall row . (Show row, CRUDRow row) => Handle -> IO (CRUD STM row)
readCRUD h = do

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

    -- load CRUD, please
    env <- loadCRUD BS.empty (HashMap.empty :: HashMap.HashMap Text row)

    print env

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

    forkIO $ forever $ do
          tu <- atomically $ readTChan updateChan
          print $ "writing" ++ show tu
          LBS.hPutStr h (encode tu)
          LBS.hPutStr h "\n" -- just for prettyness, nothing else
          hFlush h
          return ()

    return $ CRUD
     { createRow = \ row    -> do iD <- next
                                  row' <- lensID (const $ return iD) row
                                  updateCRUD (RowUpdate iD row')
                                  return row'
     , getRow    = \ iD     -> do t <- readTVar table
                                  return $ HashMap.lookup iD t
     , getTable  =             do readTVar table
     , updateRow = \ iD row -> do updateCRUD (RowUpdate iD row)
     , deleteRow = \ iD     -> do updateCRUD (RowDelete iD)
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

createCRUD :: (FromJSON row, ToJSON row) => HashMap Text row -> IO (CRUD STM row)
createCRUD = error ""

datatypeCRUD :: forall m row . (Monad m, CRUDRow row) => CRUD m row -> CRUD m Object
datatypeCRUD crud = CRUD
     { createRow = return . toObject <=< createRow crud <=< fromObject
     , getRow    = \ iD -> do optRow <- getRow crud iD
                              case optRow of
                                Nothing -> return Nothing
                                Just row -> liftM Just $ return $ toObject row
     , getTable = liftM (fmap toObject) $ getTable crud
     , updateRow = \ iD -> updateRow crud iD <=< fromObject
     , deleteRow = deleteRow crud
     }
 
 where toObject :: row -> Object
       toObject r = case toJSON r of
                  Object obj -> obj
                  _ -> error "row is not representable as an Object"
       fromObject :: Object -> m row
       fromObject obj = case fromJSON (Object obj) of
                       Success r -> return r
                       Error err -> fail err

-- | create a CRUD that does not honor write requests.
readOnlyCRUD :: (Monad m) => CRUD m row -> CRUD m row
readOnlyCRUD crud = CRUD 
     { createRow = \ iD  -> fail ""
     , getRow    = \ iD     -> getRow crud iD
     , getTable  =             getTable crud
     , updateRow = \ iD row -> fail ""
     , deleteRow = \ iD     -> fail ""
     }

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
        

main = do 
        bs <- LBS.readFile "test.json"
        let ws = parseCollection (LBS.toChunks bs)
        let ws' = ws -- Prelude.take 100000000 (cycle ws)
        print $ foldl' (flip writeCollection) HashMap.empty ws'
        print ()
        
--(LBS.toChunks lbs)

-- This is written to be lazy, to minimize space usage.
parseCollection :: [BS.ByteString] -> [RESTfulWRITE]
parseCollection [] = []
parseCollection (bs:bss) 
        | BS.null bs = parseCollection bss
        | otherwise  = p (Atto.parse P.json bs) bss
  where p (Fail bs _ msg) bss 
                | all (BS.all (isSpace . chr . fromIntegral)) (bs:bss) = []
                | otherwise = fail $ "parse error: " ++ msg
        p (Partial k) (bs:bss) 
                | BS.null bs = p (Partial k) bss
                | otherwise  = p (k bs) bss
        p (Partial k) []  = p (k BS.empty) []        
        p (Done bs r) bss = case fromJSON r of
                              Error msg -> error msg
                              Success v -> v : parseCollection (bs:bss)

loop bs = do
        print bs
        loop2 (Atto.parse P.json bs)
loop2 (Fail {}) = error "F"
loop2 (Partial k) = loop2 (k (BS.empty))
loop2 (Done t r) = do print (t,r)
                      loop t


        

--data Transaction = Transaction
--        { date :: Date
--        }

data RESTfulRequest
        = READ  RESTfulREAD
        | WRITE RESTfulWRITE
        deriving (Show, Eq)
        
data RESTfulREAD
        = GetCollection 
        | GetModel ID
        deriving (Show, Eq)
        

data RESTfulWRITE
        = UpdateModel ID Object  -- POST & PUT map to this
        | DeleteModel ID
        deriving (Show, Eq)


-- Check that the ID argument is the same as the one inside the Object.
invarientRESTfulWRITE :: RESTfulWRITE -> Bool
invarientRESTfulWRITE _ = True 

data RESTfulResponse
        = Okay
        | Redirect ID
        | ReturnModel Object
        | NotFound              -- 404
        deriving (Show, Eq)

type ID = String
        

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
instance ToJSON RESTfulWRITE where
   toJSON (UpdateModel _id obj) = Object $ HashMap.fromList [("update:",Object obj)]

----------------------------------------------------------------------
--
----------------------------------------------------------------------

-- Like an Object, but the range *must* be an Object,
-- and this object must contain the { "id": key, ... }
type Collection = HashMap ID Object

writeCollection :: RESTfulWRITE -> Collection -> Collection
writeCollection (UpdateModel key val) = HashMap.insert key val
writeCollection (DeleteModel key)     = HashMap.delete key

----------------------------------------------------------------------

-- Changes all all either an update (create a new field if needed) or a delete.

data TableUpdate row
        = RowUpdate Text row
        | RowDelete Text
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
   toJSON (RowUpdate key obj) = 
                   case toJSON obj of
                     o@(Object env) -> 
                       case HashMap.lookup "id" env of
                         Just (String key')
                            | key /= key' -> error "id: key does not match constructor"
                            | otherwise   -> o
                         Just _  -> error "id: is not a string"
                         Nothing -> error "no id: key in row"
                     _ -> error "row should be an object"
   toJSON (RowDelete key)      = Object $ HashMap.fromList [("delete",String key)]

instance FromJSON row => FromJSON (TableUpdate row) where
    parseJSON (Object v) =
        (do String iD <- case HashMap.lookup "id" v of
                     Nothing -> fail "no id"
                     Just v -> return v
            row <- parseJSON (Object v)    
            return $ RowUpdate iD row
        ) <|> 
        ( RowDelete <$>
               v .: "delete"
        )

tableUpdate :: TableUpdate row -> HashMap Text row -> HashMap Text row
tableUpdate (RowUpdate key row) = HashMap.insert key row
tableUpdate (RowDelete key)     = HashMap.delete key

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

