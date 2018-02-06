module Kubernetes where

type ObjectName = String
type ObjectNamespace = String

data DBSecret = DBSecret
  { dbSecretName :: ObjectName
  , dbSecretNamespace :: ObjectNamespace
  , dbSecretUser :: String
  , dbSecretPassword :: String
  , dbSecretHost :: String
  , dbSecretPort :: Int
  , dbSecretDatabase :: String
  , dbSecretDatabaseURL :: String
  }

data EventStatus = Create | Update | Delete

data PostgresCRDStatus = PostgresCRDStatus
  { postgresCRDStatusId :: String
  , postgresCRDStatusState :: String
  }

data PostgresCRDSpec = PostgresCRDSpec
  { postgresCRDSpecStorage :: Int
  , postgresCRDSpecSize :: String
  }

data PostgresCRD = PostgresCRD
  { postgresCRDName :: String
  , postgresCRDNamespace :: String
  , postgresCRDSpec :: PostgresCRDSpec
  , postgresCRDStatus :: PostgresCRDStatus
  }

data PostgresCRDEvent = PostgresCRDEvent EventStatus PostgresCRD

class (Monad m) => MonadKubernetes m where
  doesCRDExist :: m Bool
  getSecret :: ObjectName -> ObjectNamespace -> m (Maybe DBSecret)
  createSecret :: DBSecret -> m ()
  updateSecret :: DBSecret -> m ()
  waitForCRDEvent :: m PostgresCRDEvent
  updateCRD :: PostgresCRD -> m ()

setSecret :: (MonadKubernetes m) => DBSecret -> m ()
setSecret scrt = do
  exists <- getSecret (dbSecretName scrt) (dbSecretNamespace scrt)
  case exists of
    Just _ -> updateSecret scrt
    Nothing -> createSecret scrt
