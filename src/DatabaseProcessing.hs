{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module DatabaseProcessing (runDB, updatePlayer, updateStats, getTable) where
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import System.IO.Unsafe

import Types
import           Control.Monad.Trans

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
getTable = TopTable (unsafePerformIO getRecords)

runDB :: MonadIO m => ConnectionPool -> SqlPersistM a -> m a
runDB pool m = liftIO (runSqlPersistMPool m pool)

getRecords :: IO ([TopTableRecord])
getRecords = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        topListRecords <- selectList [TopListDraws ==. 0] [Desc TopListWins, Asc TopListLosts, LimitTo 10]
        -- recById <- getBy $ TopListNameUniq "Vasilesk"
        -- liftIO $ print recById
        -- liftIO $ print (map getPlayerStatsForTop (map getTopListForTop topListRecords))
        -- return (getPlayerStats (getTopList recById))
        return (map getPlayerStatsForTop (map getTopListForTop topListRecords))

updatePlayer :: PlayerData -> PlayerData
updatePlayer a = unsafePerformIO (do
  statsFromDB <- getStatsByName (login a)
  return (PlayerData ((login a) == "Guest") (login a) "" statsFromDB))

updateStats :: PlayerData -> SqlPersistM ()
updateStats playerData = do
        recById <- getBy $ TopListNameUniq (login playerData)
        liftIO $ print recById
        makeStatsUpdateById ( moves (stats playerData)) ( wins (stats playerData)) ( draws (stats playerData)) ( losts (stats playerData)) (getKey recById)

makeStatsUpdateById :: Int -> Int -> Int -> Int -> Key TopList -> SqlPersistM ()
-- makeStatsUpdateById m w d l key = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
--     flip runSqlPersistMPool pool $ do
--         runMigration migrateAll
--
--         update key [TopListMoves =. m, TopListWins =. w, TopListDraws =. d, TopListLosts =. l]
--         return ()
makeStatsUpdateById m w d l key = update key [TopListMoves =. m, TopListWins =. w, TopListDraws =. d, TopListLosts =. l]

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

getTopListForTop :: (Entity TopList) -> TopList
getTopListForTop (Entity key value) = value



getKey :: Maybe (Entity TopList) -> Key TopList
getKey (Just (Entity key value)) = key

getPlayerStats :: Maybe TopList -> PlayerStats
getPlayerStats (Just (TopList _ m w d l)) =  (PlayerStats m w d l)
getPlayerStats Nothing = (PlayerStats 0 0 0 0)

getPlayerStatsForTop :: TopList -> TopTableRecord
getPlayerStatsForTop (TopList n m w d l) =  (TopTableRecord n (PlayerStats m w d l))

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





-- parser2 :: TopList -> String
-- parser2 (TopList st _ _ _ _) = st

-- fetchName :: Maybe TopList -> String
-- fetchName Nothing = "No user with the name!"
-- fetchName (Just (TopList st _ _ _ _)) = st
--
-- runDb :: IO ()
-- runDb = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
--     flip runSqlPersistMPool pool $ do
--         runMigration migrateAll
--
--         johnId <- insert $ Person "John Doe" $ Just 35
--         janeId <- insert $ Person "Jane Doe" Nothing
--
--         insert $ BlogPost "My fr1st p0st" johnId
--         insert $ BlogPost "One more for good measure" johnId
--
--         oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
--         liftIO $ print (oneJohnPost :: [Entity BlogPost])
--         -- [Entity {entityKey = BlogPostKey {unBlogPostKey = SqlBackendKey {unSqlBackendKey = 11}}, entityVal = BlogPost {blogPostTitle = "My fr1st p0st", blogPostAuthorId = PersonKey {unPersonKey = SqlBackendKey {unSqlBackendKey = 11}}}}]
--
--         john <- get johnId
--         liftIO $ print (john :: Maybe Person)
--         -- Just (Person {personName = "John Doe", personAge = Just 35})
--
--         -- personId <- insert $ Person2 "Michael" "Snoyman" 26
--         -- maybePerson <- getBy $ Person2Name "Michael" "Snoyman"
--         -- case maybePerson of
--         --     Nothing -> liftIO $ putStrLn "Just kidding, not really there"
--         --     Just (Entity personId person) -> liftIO $ print person
--         --     -- Person2 {person2FirstName = "Michael", person2LastName = "Snoyman", person2Age = 26}
--
--         delete janeId
--         deleteWhere [BlogPostAuthorId ==. johnId]
