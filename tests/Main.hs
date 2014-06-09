{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeOperators, TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs #-}

module Main where

import qualified Data.Text as Text
import Data.Text(Text,pack)
import Web.Scotty.CRUD
import System.IO
import System.Directory
import Control.Applicative
import Control.Monad
import Control.Lens ((^.))
import qualified Data.HashMap.Strict as HashMap
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.QuickCheck.Function
--TMP
import Data.Aeson

import QC


-- Simple tests
-- Saving, then loading again, will get back to the same CRUD.

data CRUDAction row a where
   -- CRUD
   CreateRow :: row -> CRUDAction row () 
   GetRow    :: Text -> CRUDAction row ()       -- auto-compared
   UpdateRow :: row -> CRUDAction row ()
   -- Hack to get round the function QC function issue (TODO: revisit)
   -- Gets a stored ID. O => a new name, 1 => first, 2 => 2nd Create, etc.
   GetId     :: Int -> CRUDAction row Text
   -- External
   Restart   :: CRUDAction row ()
   -- Assert
   Assert    :: Bool -> String -> CRUDAction row ()
   -- Monad
   Bind      :: CRUDAction row a -> (a -> CRUDAction row b) -> CRUDAction row b
   Return    :: a -> CRUDAction row a

instance Monad (CRUDAction row) where
        (>>=) = Bind
        return = Return

instance Applicative (CRUDAction row) where
        (<*>) = liftM2 ($)
        pure = Return
        
instance Functor (CRUDAction row) where
        fmap f act = pure f <*> act

instance Show row => Show (CRUDAction row a) where
        show (CreateRow row) = "CreateRow {..}" -- ++ show row
        show (GetRow iD)     = "GetRow " ++ show iD
        show (GetId n)       = "GetId " ++ show n
        show (Restart)       = "Restart "
        
        show (Bind m@(CreateRow {}) k) = show m ++ ";\n" ++ show (k ())
        show (Bind m@(GetRow {}) k)    = show m ++ ";\n" ++ show (k ())
        show (Bind m@(GetId n) k)      = show m ++ ";\n" ++ show (k (pack $ "<id-" ++ show n ++ ">"))
        show (Bind m@(Restart {}) k)   = show m ++ ";\n" ++ show (k ())
        show (Return _) = "Return"

data Env row = Env 
        { theCRUD  :: CRUD IO row
        , fileName :: FilePath
        , handle   :: Handle
        , ids      :: [Text]
        , oracle   :: [(Text,row)]
        }

interpBind :: (CRUDRow row, Show row, Eq row) => CRUDAction row a -> (a -> CRUDAction row b) ->  Env row -> IO Bool
interpBind (GetRow iD) k env = do
        putStrLn $ "GetRow " ++ show iD
        ans <- getRow (theCRUD env) iD
        case (ans,lookup iD (oracle env)) of
          (a1,a2) | a1 == a2 -> interp (k ()) env
          _ -> return False
interpBind (CreateRow row) k env = do
        putStrLn $ "CreateRow " ++ show row
        row' <- createRow (theCRUD env) row
        let iD' = row' ^.lensID
        let env' = env { oracle = (iD',row') : [ (k,v) | (k,v) <- oracle env, k /= iD' ] 
                       , ids = ids env ++ [iD']
                       }
        interp (k ()) env'
interpBind (GetId n) k env
        | n > 0 = interp (k (ids env !! (n - 1))) env
        | n == 0 = interp (k iD) env
   where iD = head [ t :: Text
                   | n <- [1..] :: [Int]
                   , let t = Text.pack (show n)
                   , not (t `elem` (ids env))
                   ]
interpBind (Restart) k env = do
        sync (theCRUD env)       -- wait until it is all done
        -- Flush the buffer
        hFlush (handle env)
        hClose (handle env)
        h <- openBinaryFile test_json ReadWriteMode
        crud <- readCRUD h
        interp (k ()) (env { handle = h, theCRUD = atomicCRUD crud })
        

interpBind (Assert b msg) k env = do
        if b
        then interp (k ()) env
        else do putStrLn $ "Assert fail" ++ msg
                return False
        
interpBind (Return a) k env = interp (k a) env
interpBind (Bind m k2) k env = interpBind m (\ r -> Bind (k2 r) k) env
interpBind other k env = error $ "interpBind: " ++ show other

interp :: (CRUDRow row, Show row, Eq row) => CRUDAction row a ->  Env row -> IO Bool
interp (Bind m k) env = interpBind m k env
interp (Return _) env = return True
interp other      env = interpBind other Return env

test_json = "tmp/test.json" :: String

main = quickCheck prop_crud
{-
        -- First, clear the start
        b <- doesFileExist test_json
        if b
        then removeFile test_json
        else return ()
        h <- openBinaryFile test_json ReadWriteMode
        crud <- readCRUD h
        r <- interp act $ Env (atomicCRUD crud) test_json h [] []
        print r
-}

runCRUDAction :: CRUDAction Object () -> IO Bool
runCRUDAction prog = do
        -- First, clear the start
        b <- doesFileExist test_json
        if b
        then removeFile test_json
        else return ()
        h <- openBinaryFile test_json ReadWriteMode
        crud <- readCRUD h
        interp prog $ Env (atomicCRUD crud) test_json h [] []

prop_crud :: Property
prop_crud = monadicIO $ do
        code <- pick (generateTest 10 0)
        run $ putStrLn "\n"
        run $ print code
        ans <- run $ runCRUDAction code
        assert ans
        return ()      


act :: CRUDAction Object ()
act = do
      CreateRow (HashMap.fromList [("x", Number 1),("y", Number 2)])  
      id1 <- GetId 1
      GetRow id1
      GetRow "undefined"
      CreateRow (HashMap.fromList [("x", Number 1),("y", Number 2)])  
      id2 <- GetId 2
      Assert (id1 /= id2) "ids from create should be unique"


{-
         
generateID :: Gen ([Text] :-> Text)
generateID = do
        i <- choose (0,101)
        return $ function $ \ xs -> 
            cycle (head [ t
                        | n <- [1..]
                        , let t = Text.pack (show n)
                        , not (t `elem` xs)
                        ] : xs) !! i
-}

generateTest :: Int -> Int -> Gen (CRUDAction Object ())
generateTest n idCount = frequency 
        [ ( if n > 0 then 10 else 0
          , do row <- arbitraryHashMap False 5
               rest <- generateTest (n-1) (idCount + 1)
               return $ CreateRow row >>= \ () -> rest
          )
        , ( if n > 0 then 10 else 0
          , do r <- choose (0,idCount)
               rest <- generateTest (n-1) idCount
               return $ GetId r >>= \ iD -> GetRow iD >>= \ () -> rest
          )
        , ( if n > 0 then 5 else 0
          , do rest <- generateTest (n-1) idCount
               return $ Restart >>= \ () -> rest
          )
        , (1,return (return ()))
        ]

