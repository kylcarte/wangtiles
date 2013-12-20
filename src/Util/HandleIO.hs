
module Util.HandleIO where

import Error
import Random
import Util

import qualified System.Random as R
import System.Exit

class (Monad m) => HandleIO m where
  io :: String -> m a -> IO a
  io' :: m a -> IO a
  io' = io "HandleIO failure"

instance HandleIO Random where
  io _ m = do
    g <- R.getStdGen
    let (a,g') = runRandom m g
    R.setStdGen g'
    return a
  io'  = io "failure in Random"

instance (HandleIO m) => HandleIO (ErrorT m) where
  io msg m = do
    e <- io msg $ runErrorT m
    case e of
      Left err  -> do putStrLn $ addLine msg err
                      exitFailure
      Right res -> return res
  io' = io "failure in Error"

instance HandleIO Identity where
  io _ = return . runIdentity
  io'  = return . runIdentity

instance HandleIO IO where
  io _ = id
  io'  = id

instance HandleIO Maybe where
  io msg = maybe (fail msg) return

