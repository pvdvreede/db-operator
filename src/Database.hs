{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Database where

import Control.Monad.Free
import Control.Monad.Free.TH
import Text.Printf

type Username = String
type Password = String

data Connection = Connection
  { connectionHost :: String
  , connectionPort :: Int
  , connectionUser :: Username
  , connectionPassword :: Password
  , connectionDatabase :: String
  }

instance Show Connection where
  show (Connection h p u pass db) = printf "postgres://%s:%s@%s:%i/%s?sslmode=require" u pass h p db

data Database = Database
  { databaseName :: String
  , databaseStorage :: Int
  , databaseSize :: String
  , databaseAdminConnection :: Connection
  } deriving Show

data DatabaseRequest = DatabaseRequest
  { databaseRequestName :: String
  , databaseRequestStorage :: Int
  , databaseRequestSize :: String
  , databaseRequestAdminUser :: Username
  , databaseRequestAdminPassword :: Password
  } deriving Show

data DatabaseAPI next =
  GetDatabase String (Maybe Database -> next)
  | GeneratePassword (Password -> next)
  | CreateDatabaseServer DatabaseRequest (Database -> next)
  | CreateDatabaseUser Connection Username Password next
  deriving Functor

makeFree ''DatabaseAPI

type DatabaseProgram = Free DatabaseAPI

newDatabaseRequest :: String -> Int -> String -> DatabaseProgram DatabaseRequest
newDatabaseRequest name storage size = do
  pw <- generatePassword
  return $ DatabaseRequest name storage size "postgres" pw

createDatabaseIfNotExist :: DatabaseRequest -> DatabaseProgram Database
createDatabaseIfNotExist db = do
  getdb <- getDatabase (databaseRequestName db)
  case getdb of
    Just existDb -> return existDb
    Nothing -> do
      newDb <- createDatabaseServer db
      return newDb

createDatabaseUsers :: [Username] -> Connection -> DatabaseProgram [(Username, Password)]
createDatabaseUsers users conn = do
    mapM createUser users
  where
    createUser user = do
      pw <- generatePassword
      createDatabaseUser conn user pw
      return (user, pw)
