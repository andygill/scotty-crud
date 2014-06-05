module Web.Scotty.CRUD where

import Web.Scotty
import Data.Aeson
import Data.Aeson.Parser as P
import Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as BS


-- | crud takes a directory path, and a URL, and returns
-- a scotty monad that provides a RESTful CRUD for this URL collection.

crud :: FilePath -> String -> ScottyM ()
crud = undefined

main = do 
        bs <- BS.readFile "test.json"
        loop bs
        
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

data RESTfulREAD
        = GetCollection 
        | GetModel ID

data RESTfulWRITE
        = CreateModel Value
        | UpdateModel ID Value
        | DeleteModel ID

data RESTfulResponse
        = Okay
        | Redirect ID
        | ReturnModel Value
        | NotFound              -- 404

data ID = String
        
        