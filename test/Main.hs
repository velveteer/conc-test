{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import qualified Control.Concurrent.Classy as Conc
import qualified Control.Concurrent.Class.MonadMVar as MVar
import qualified Control.Concurrent.Class.MonadSTM.TVar as TVar
import qualified Control.Monad.IO.Class as MIO
import qualified Control.Monad.Class.MonadFork as Fork
import qualified Control.Monad.Class.MonadSay as Say
import qualified Control.Monad.Class.MonadTimer as Timer
import qualified Control.Monad.Class.MonadTime as TimeM
import qualified Control.Monad.Class.MonadSTM as STM
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IOSim as Sim
import qualified Data.Either as Either
import qualified Data.IORef as IORef
import qualified Data.Time as Time
import qualified Test.HUnit as HU
import qualified Test.Hspec.Contrib.HUnit as HU
import qualified Test.HUnit.DejaFu as HUD
import qualified Test.Hspec as HS
import qualified Test.Hspec.QuickCheck as HS
import qualified Test.QuickCheck as QC

import qualified Conc as Lib

main :: IO ()
main = HS.hspec $ do
  racyTests
  dejaTests
  iosimTests

racyTests :: HS.SpecWith ()
racyTests =

  HS.describe "racy" $ do
    HS.it "should be consistent" $ do
      x <- Lib.racyIO
      y <- Lib.racyIO
      x `HS.shouldBe` y

    HS.prop "should be consistent" $ do
      QC.again . QC.ioProperty $ do
        x <- Lib.racyIO
        y <- Lib.racyIO
        pure $ x QC.=== y

    HU.fromHUnitTest $ HUD.testAuto Lib.racyConc

dejaTests :: HS.SpecWith ()
dejaTests = do

  HS.describe "Auth Token Initialization (DejaFu)" $ do
    HU.fromHUnitTest $ HUD.testAuto $ do

      let refresh countRef = do
            expiry <-
              Time.addUTCTime Time.nominalDay <$>
                MIO.liftIO Time.getCurrentTime
            Conc.threadDelay 1000000
            _ <- Conc.atomicModifyIORef countRef $ \a -> (a + 1, ())
            pure ("hello", expiry)

      let setup = do
            refreshCount <- Conc.newIORef (0 :: Int)
            tokenVar <- Conc.newEmptyMVar
            initLock <- Conc.newEmptyMVar
            HUD.registerInvariant $ do
                value <- HUD.inspectIORef refreshCount
                Monad.when (value > 1) (fail "token was refreshed more than once")
            pure (tokenVar, initLock, refreshCount)

      HUD.withSetup setup $ \(tokenVar, initLock, countRef) -> do
        now <- MIO.liftIO Time.getCurrentTime
        _
          <- Conc.fork
           . Monad.void
           $ Lib.getTokenConc now tokenVar initLock (refresh countRef)

        Monad.void $ Lib.getTokenConc now tokenVar initLock (refresh countRef)

  HS.describe "Auth Token Concurrent Error (DejaFu)" $ do
    HU.fromHUnitTest $ HUD.testAuto $ do

      let refresh = do
            expiry <-
              Time.addUTCTime Time.nominalDay <$>
                MIO.liftIO Time.getCurrentTime
            Conc.threadDelay 1000000
            pure ("hello", expiry)

      let setup = do
            tokenVar <- Conc.newEmptyMVar
            initLock <- Conc.newEmptyMVar
            pure (tokenVar, initLock)

      HUD.withSetup setup $ \(tokenVar, initLock) -> do
        now <- MIO.liftIO Time.getCurrentTime
        _
          <- Conc.fork
           . Monad.void
           $ Lib.getTokenConc now tokenVar initLock
              (Catch.throwM $ userError "error")

        Monad.void $ Lib.getTokenConc now tokenVar initLock refresh

  HS.describe "Auth Token Concurrent Refresh (DejaFu)" $ do
    HU.fromHUnitTest $ HUD.testAuto $ do

      let refresh = do
            expiry <-
              Time.addUTCTime Time.nominalDay <$>
                MIO.liftIO Time.getCurrentTime
            -- Note: deja fu does not model TIME. This means a thread delay is
            -- implemented as a yield. See the io-sim version of this test,
            -- where we must deal with time explicitly.
            -- Conc.threadDelay 1000000
            Conc.yield
            pure ("hello", expiry)

      let setup = do
            tokenVar <- Conc.newEmptyMVar
            initLock <- Conc.newEmptyMVar
            pure (tokenVar, initLock)

      HUD.withSetup setup $ \(tokenVar, initLock) -> do
        now <- MIO.liftIO Time.getCurrentTime
        Monad.void $ Lib.getTokenConc now tokenVar initLock refresh
        _
          <- Conc.fork
           . Monad.void
           $ Lib.getTokenConc (Time.addUTCTime Time.nominalDay now) tokenVar initLock refresh
        Monad.void $ Lib.getTokenConc now tokenVar initLock refresh

iosimTests :: HS.SpecWith ()
iosimTests = do

  HS.describe "Auth Token Initialization (IOSim)" $ do
    let test :: Sim.IOSim s Int
        test = do
          let
            refresh countVar = do
                expiry <-
                  Time.addUTCTime Time.nominalDay <$> TimeM.getCurrentTime
                STM.atomically $ TVar.modifyTVar' countVar $ \a -> a + 1
                pure ("hello", expiry)

          refreshCount <- TVar.newTVarIO 0
          tokenVar <- MVar.newEmptyMVar
          initLock <- MVar.newEmptyMVar
          now <- TimeM.getCurrentTime
          _
            <- Fork.forkIO
             . Monad.void
             $ Lib.getTokenIOSim now tokenVar initLock (refresh refreshCount)
          Monad.void $ Lib.getTokenIOSim now tokenVar initLock (refresh refreshCount)
          STM.atomically $ TVar.readTVar refreshCount

    HS.it "should refresh only once" $ do
      Sim.runSimOrThrow test `HS.shouldBe` 1

  HS.describe "Auth Token Concurrent Error (IOSim)" $ do
    let test :: Sim.IOSim s ()
        test = do
          let
            refresh :: (Timer.MonadDelay m, TimeM.MonadTime m) => m Lib.Token
            refresh = do
                expiry <-
                  Time.addUTCTime Time.nominalDay <$> TimeM.getCurrentTime
                pure ("hello", expiry)

          tokenVar <- MVar.newEmptyMVar
          initLock <- MVar.newEmptyMVar
          now <- TimeM.getCurrentTime
          _
            <- Fork.forkIO
             . Monad.void
             $ Lib.getTokenIOSim now tokenVar initLock (Catch.throwM $ userError "uh oh")

          Monad.void $ Lib.getTokenIOSim now tokenVar initLock refresh

    HS.prop "does not deadlock" $ do
      Sim.exploreSimTrace id test assertNoDeadlock

  HS.describe "Auth Token Concurrent Refresh (IOSim)" $ do
    let test :: Sim.IOSim s ()
        test = do
          let
            refresh :: (Timer.MonadDelay m, TimeM.MonadTime m) => m Lib.Token
            refresh = do
                expiry <- Time.addUTCTime Time.nominalDay <$> TimeM.getCurrentTime
                Timer.threadDelay 1000000
                -- Fork.yield -- NOT equivalent to a delay here
                pure ("hello", expiry)

          tokenVar <- MVar.newEmptyMVar
          initLock <- MVar.newEmptyMVar
          now <- TimeM.getCurrentTime
          Monad.void $ Lib.getTokenIOSim now tokenVar initLock refresh
          _
            <- Fork.forkIO . Monad.void $ do
              Lib.getTokenIOSim (Time.addUTCTime Time.nominalDay now) tokenVar initLock refresh
          -- Timer.threadDelay 500000
          Monad.void $ Lib.getTokenIOSim now tokenVar initLock refresh

    -- HS.it "does not deadlock" $ Sim.runSimOrThrow test `HS.shouldBe` ()
    HS.prop "does not deadlock" $ do
      Sim.exploreSimTrace id test assertNoDeadlock

assertNoDeadlock :: Maybe (Sim.SimTrace a) -> Sim.SimTrace a -> IO ()
assertNoDeadlock _ trace = do
  case Sim.traceResult True trace of
    Left (Sim.FailureDeadlock threads) -> do
      HU.assertFailure $ show threads
    _ -> pure ()
