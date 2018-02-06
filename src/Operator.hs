{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Operator where

import Control.Monad.Free
import Control.Monad.Free.TH
import Text.Printf

type DatabaseId = String
type CrdName = String

data CrdStatus = CrdStatus
  { id :: String
  , state :: String
  }

data PostgresDb = PostgresDb
  { } deriving Show

data DbSecret = DbSecret
  { dbSecretName :: String
  , dbSecretNamespace :: String
  , dbSecretHost :: String
  , dbSecretPort :: Int
  , dbSecretDatabase :: String
  }

instance Show DbSecret where
  show (DbSecret n ns h p d) = printf "%s %s %s %s" n ns h (show p)

data Database = Database
  { databaseId :: DatabaseId
  , databaseSize :: Int
  } deriving Show

data LogLevel = Debug | Info | Warn | Error deriving Show

data DbOperatorActions next =
    GetDatabase DatabaseId (Maybe Database -> next)
  | CreateDatabase Database (Database -> next)
  | UpdateCrdState PostgresDb String next
  | GetSecret String (Maybe DbSecret -> next)
  | CreateSecret DbSecret next
  | UpdateSecret DbSecret next
  | WatchPostgresDbs (PostgresDb -> next)
  deriving (Functor)

makeFree ''DbOperatorActions

type DbOperator = Free DbOperatorActions

createDatabaseIfNotExist :: Database -> DbOperator Database
createDatabaseIfNotExist db = do
  getdb <- getDatabase (databaseId db)
  case getdb of
    Just existDb -> return existDb
    Nothing -> do
      newDb <- createDatabase db
      return newDb

setSecret :: DbSecret -> DbOperator ()
setSecret scrt = do
  currentScrt <- getSecret $ dbSecretName scrt
  case currentScrt of
    Just _ -> updateSecret scrt
    Nothing -> createSecret scrt

onCrdCreate :: PostgresDb -> DbOperator ()
onCrdCreate crd = do
  updateCrdState crd "Creating database..."
  db <- createDatabaseIfNotExist (Database "test" 22)
  updateCrdState crd "Available"
  setSecret (DbSecret "a" "b" "c" 1234 "d")
  setSecret (DbSecret "e" "f" "g" 1234 "h")

startWatching :: DbOperator ()
startWatching = do
  db <- watchPostgresDbs
  onCrdCreate db
  startWatching
