module Main where

import Control.Concurrent
import Database
import Kubernetes
import Operator


instance MonadPostgresDatabase IO where
  getDatabase id = do
    putStrLn "Getting database..."
    return $ Nothing
  createDatabaseServer req = do
    putStrLn "creating DB"
    return (Database "test" 12 "last" (Connection "a" 0 "b" "c" "d"))
  generatePassword  = return "mypasswd"

instance MonadKubernetes IO where
  doesCRDExist = return False
  getSecret n ns = do
    putStrLn "Getting secret"
    return Nothing
  createSecret scrt = do
    putStrLn "Creating secret..."
  updateSecret scrt = do
    putStrLn "Updating secret..."
  waitForCRDEvent = do
    threadDelay 10000000
    return $ PostgresCRDEvent Create (PostgresCRD "name" "ns" (PostgresCRDSpec 12 "large") (PostgresCRDStatus "" ""))
  updateCRD crd = do
    putStrLn "Updating CRD..."


main :: IO ()
main = startWatching
