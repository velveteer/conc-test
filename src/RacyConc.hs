module RacyConc where

import qualified Control.Concurrent.Classy as Conc

racyConc :: Conc.MonadConc m => m (Maybe String)
racyConc = do
  var <- Conc.newEmptyMVar
  _ <- Conc.fork (Conc.putMVar var "hello")
  Conc.threadDelay 10
  Conc.tryTakeMVar var
