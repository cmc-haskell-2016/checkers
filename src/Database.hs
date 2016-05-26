{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database (updatePlayer, updateStats, getTable) where
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

getTable :: TopTable
getTable = undefined

updatePlayer :: PlayerData -> PlayerData
updatePlayer a = unsafePerformIO (do
  statsFromDB <- getStatsByName (login a)
  return (PlayerData ((login a) == "Guest") (login a) "" statsFromDB))

updateStats :: PlayerData -> IO ()
updateStats playerData = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        recById <- getBy $ TopListNameUniq (login playerData)
        liftIO $ print recById
        liftIO $ makeStatsUpdateById ( moves (stats playerData)) ( wins (stats playerData)) ( draws (stats playerData)) ( losts (stats playerData)) (getKey recById)
        return ()

makeStatsUpdateById :: Int -> Int -> Int -> Int -> Key TopList -> IO ()
makeStatsUpdateById m w d l key = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        update key [TopListMoves =. m, TopListWins =. w, TopListDraws =. d, TopListLosts =. l]
        return ()

getStatsByName :: String -> IO (PlayerStats)
getStatsByName name = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        recById <- getBy $ TopListNameUniq name
        liftIO $ if (testRes recById) then (print "Player was found!") else (insertRecord name (PlayerStats 0 0 0 0))
        return (getPlayerStats (getTopList recById))

testRes :: Maybe (Entity TopList) -> Bool
testRes Nothing = False
testRes _ = True

getTopList :: Maybe (Entity TopList) -> Maybe TopList
getTopList (Just (Entity key value)) = Just value
getTopList Nothing = Nothing

getKey :: Maybe (Entity TopList) -> Key TopList
getKey (Just (Entity key value)) = key

getPlayerStats :: Maybe TopList -> PlayerStats
getPlayerStats (Just (TopList _ m w d l)) =  (PlayerStats m w d l)
getPlayerStats Nothing = (PlayerStats 0 0 0 0)

-- processStats :: Maybe PlayerStats -> IO(PlayerStats)
-- processStats Just a = liftIO a


insertRecord :: String -> PlayerStats -> IO ()
insertRecord name (PlayerStats pMoves pWins pDraws pLosts) = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        insert_ $ TopList name pMoves pWins pDraws pLosts

-- getPlayerName :: Maybe TopList -> String
-- getPlayerName (Just (TopList st _ _ _ _)) = st
-- getPlayerName Nothing = "Don`t know you!"

-- processPlayerName :: String -> IO (String)
-- processPlayerName name = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
--     flip runSqlPersistMPool pool $ do
--         runMigration migrateAll
--
--         recById <- getBy $ TopListNameUniq name
--         return (getPlayerName (getTopList recById))

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
