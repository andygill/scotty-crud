{-# LANGUAGE OverloadedStrings #-}
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
import Data.Char (isSpace, chr)
import Data.List (foldl')

-- | crud takes a directory path, and a URL, and returns
-- a scotty monad that provides a RESTful CRUD for this URL collection.

crud :: FilePath -> String -> ScottyM ()
crud dir url = do
        
        get (capture url) $ do 
                return ()
        
        get (capture $ url ++ "/:id") $ do return ()
                
        

main = do 
        bs <- LBS.readFile "test.json"
        let ws = parseCollection (LBS.toChunks bs)
        let ws' = ws -- Prelude.take 100000000 (cycle ws)
        print $ foldl' (flip writeCollection) HashMap.empty ws'
        print ()
        
--(LBS.toChunks lbs)

-- This is written to be lazy, to minimize space usage.
parseCollection :: [BS.ByteString] -> [RESTfulWRITE]
parseCollection (bs:bss) | BS.null bs = parseCollection bss
parseCollection (bs:bss) = p (Atto.parse P.json bs) bss
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

