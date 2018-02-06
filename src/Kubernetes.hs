{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Kubernetes where

import Control.Monad.Free
import Control.Monad.Free.TH
import Text.Printf

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

data KubernetesAPI next =
  GetDBSecret ObjectName ObjectNamespace (DBSecret -> next)
  | CreateSecret DBSecret next
  | UpdateSecret DBSecret next
  | WaitForPostgresCRDEvents (PostgresCRDEvent -> next)
  | UpdatePostgresCRD PostgresCRD next
  | DoesPostgresCRDExist (Bool -> next)
  | CreatePostgresCRDType next
  deriving Functor

makeFree ''KubernetesAPI

type KubernetesProgram = Free KubernetesAPI
