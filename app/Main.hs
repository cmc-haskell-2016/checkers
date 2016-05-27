{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

-- import Lib
import Types
import WorldProcessing
import GraphicsProcessing

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import DatabaseProcessing(runDB)

connStr = "host=localhost dbname=haskell_checkers user=haskell_checkers password=hc12345678 port=5433"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
Person2
    firstName String
    lastName String
    age Int
    Person2Name firstName lastName
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
TopList
    name String
    moves Int
    wins Int
    draws Int
    losts Int
    TopListNameUniq name
    deriving Show
|]


main :: IO ()
main = do
  print "hi!"
  runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    runDB pool $ runMigration migrateAll
    gameStart pool
