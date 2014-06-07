{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs #-}

module Main where

import Web.Scotty.CRUD
\n-- Simple tests
-- Saving, then loading again, will get back to the same CRUD.

data CRUDACtion row a where
   CreateRow :: row -> CRUDAction row
   GetRow    :: Text -> CRUDAction (Maybe row)

{-
   createRow :: row         -> m row
       , getRow    :: Text        -> m (Maybe row)
       , getTable                 :: m (HashMap Text row)
       , updateRow :: Text -> row -> m ()
       , deleteRow :: Text        -> m () -- alway works
       }
-}