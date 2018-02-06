{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Operator where

import Control.Monad.Free
import Control.Monad.Trans.Free
import Control.Monad.Free.TH

import Database
import Kubernetes

data DBOperatorProgram a b = FreeT (DatabaseProgram a) (KubernetesProgram b) ()

onCRDCreate :: PostgresCRDEvent -> DBOperatorProgram () ()
onCRDCreate crd = do
  req <- liftF $ newDatabaseRequest "test" 10 "test"
  return ()
