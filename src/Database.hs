module Database where

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

class Monad m => MonadPostgresDatabase m where
  getDatabase :: String -> m (Maybe Database)
  createDatabaseServer :: DatabaseRequest -> m Database
  generatePassword :: m Password

newDatabaseRequest :: MonadPostgresDatabase m => String -> Int -> String -> m DatabaseRequest
newDatabaseRequest name storage size = do
  pw <- generatePassword
  return (DatabaseRequest name storage size "postgres" pw)

createDatabaseIfNotExists :: MonadPostgresDatabase m => DatabaseRequest -> m Database
createDatabaseIfNotExists req = do
  existing <- getDatabase (databaseRequestName req)
  case existing of
    Just db -> return db
    Nothing -> do
      newDB <- createDatabaseServer req
      return newDB
