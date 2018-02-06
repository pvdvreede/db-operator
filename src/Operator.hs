{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Operator where

import Database
import Kubernetes

convertConnectionToDBSecret :: Connection -> DBSecret
convertConnectionToDBSecret conn = undefined

onCRDCreated :: (MonadPostgresDatabase m, MonadKubernetes m) => PostgresCRD -> m ()
onCRDCreated crd = do
  req <- newDatabaseRequest (postgresCRDName crd) 10 "smalL"
  db <- createDatabaseIfNotExists req
  setSecret (convertConnectionToDBSecret $ databaseAdminConnection db)

startWatching :: (MonadPostgresDatabase m, MonadKubernetes m) => m ()
startWatching = do
  event <- waitForCRDEvent
  case event of
    PostgresCRDEvent Create crd -> do
      onCRDCreated crd
      startWatching
    _ -> startWatching
