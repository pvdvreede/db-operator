module Main where

import Control.Monad.Free
import Control.Concurrent
import Operator

stdoutInterpreter :: DbOperatorActions (IO next) -> IO next
stdoutInterpreter action = case action of
  (WatchPostgresDbs next) -> do
    putStrLn "Watching for CRDS..."
    threadDelay 10000000
    next (PostgresDb)
  (GetDatabase id next) -> do
    putStrLn "Getting database..."
    next $ Just (Database id 22)
  (CreateDatabase db next) -> do
    putStrLn "Creating database..."
    next db
  (UpdateCrdState crd state next) -> do
    putStrLn "Updating CRD state..."
    next
  (GetSecret name next) -> do
    putStrLn "Getting Secret..."
    next $ Just (DbSecret "a" "b" "c" 1234 "d")
  (CreateSecret scrt next) -> do
    putStrLn "Creating Secret..."
    next
  (UpdateSecret scrt next) -> do
    putStrLn "Updating Secret..."
    next

main :: IO ()
main = iterM stdoutInterpreter $ startWatching
