{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database (processPlayerName) where
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import System.IO.Unsafe

import Types

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

connStr = "host=localhost dbname=haskell_checkers user=haskell_checkers password=hc12345678 port=5433"

processPlayerName :: String -> IO (String)
processPlayerName name = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        recById <- getBy $ TopListNameUniq name
        return (getPlayerName (getTopList recById))

updatePlayer :: PlayerData -> PlayerData
updatePlayer a = unsafePerformIO (do
  name <- processPlayerName "Guest"
  isAGuest <- (name == "Guest")
  return (PlayerData isAGuest (login a) "" (stats a)))

getStatsByName :: String -> PlayerStats
getStatsByName = undefined

getTable :: TopTable
getTable = undefined

-- addGuest :: IO (String)
-- addGuest = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
--     flip runSqlPersistMPool pool $ do
--         runMigration migrateAll
--
--         -- oneRec <- selectList [TopListName ==. "Guest"] [LimitTo 1]
--         -- liftIO $ print (oneRec :: [Entity TopList])
--         -- insert $ TopList "Guest" 0 0 0 0
--
--         -- get topListId
--
--         -- topListId <- insert $ TopList "Guest" 0 0 0 0
--         -- recById <- get topListId
--
--         recById <- getBy $ TopListNameUniq "Guest"
--         -- liftIO $ print (getPlayerName (getTopList recById))
--         return (getPlayerName (getTopList recById))
--
--         -- insert $ TopList "Guest" 0 0 0 0
--         -- topListId <- insert $ TopList "Guest" 0 0 0 0
--         -- topList <- get topListId
--         -- delete topListId

getTopList :: Maybe (Entity TopList) -> Maybe TopList
getTopList (Just (Entity key value)) = Just value
getTopList Nothing = Nothing

getPlayerName :: Maybe TopList -> String
getPlayerName (Just (TopList st _ _ _ _)) = st
getPlayerName Nothing = "Fuck you!"



parser2 :: TopList -> String
parser2 (TopList st _ _ _ _) = st

-- fetchName :: Maybe TopList -> String
-- fetchName Nothing = "No user with the name!"
-- fetchName (Just (TopList st _ _ _ _)) = st

runDb :: IO ()
runDb = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing

        insert $ BlogPost "My fr1st p0st" johnId
        insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])
        -- [Entity {entityKey = BlogPostKey {unBlogPostKey = SqlBackendKey {unSqlBackendKey = 11}}, entityVal = BlogPost {blogPostTitle = "My fr1st p0st", blogPostAuthorId = PersonKey {unPersonKey = SqlBackendKey {unSqlBackendKey = 11}}}}]

        john <- get johnId
        liftIO $ print (john :: Maybe Person)
        -- Just (Person {personName = "John Doe", personAge = Just 35})

        -- personId <- insert $ Person2 "Michael" "Snoyman" 26
        -- maybePerson <- getBy $ Person2Name "Michael" "Snoyman"
        -- case maybePerson of
        --     Nothing -> liftIO $ putStrLn "Just kidding, not really there"
        --     Just (Entity personId person) -> liftIO $ print person
        --     -- Person2 {person2FirstName = "Michael", person2LastName = "Snoyman", person2Age = 26}

        delete janeId
        deleteWhere [BlogPostAuthorId ==. johnId]
