module RacyIO where

import qualified Control.Concurrent.MVar as MV
import qualified Control.Concurrent as C

racyIO :: IO (Maybe String)
racyIO = do
  var <- MV.newEmptyMVar
  _ <- C.forkIO (MV.putMVar var "hello")
  C.threadDelay 10
  MV.tryTakeMVar var
