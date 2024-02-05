module TokenSim where

import qualified Control.Concurrent.Class.MonadMVar as MVar
import qualified Data.Time as Time

import Common

getTokenIOSim
  :: (MVar.MonadMVar m)
  => Time.UTCTime
  -> MVar.MVar m Token
  -> MVar.MVar m ()
  -> m Token
  -> m Token
getTokenIOSim now storedToken initLock refreshToken = do
  mToken <- MVar.tryReadMVar storedToken
  case mToken of
    Nothing -> do
      MVar.putMVar initLock ()
      mToken' <- MVar.tryReadMVar storedToken
      case mToken' of
        Just mToken'' -> do
          _ <- MVar.tryTakeMVar initLock
          pure mToken''
        Nothing -> do
          token <- refreshToken
          MVar.putMVar storedToken token
          _ <- MVar.tryTakeMVar initLock
          pure token
    Just token ->
      case checkExpiry now token of
        IsOk -> pure token
        IsExpired ->
          MVar.modifyMVar storedToken $ \oaToken ->
            case checkExpiry now oaToken of
              IsOk -> pure (oaToken, oaToken)
              IsExpired -> do
                newToken <- refreshToken
                pure (newToken, newToken)

getTokenIOSimFixed
  :: (MVar.MonadMVar m)
  => Time.UTCTime
  -> MVar.MVar m Token
  -> MVar.MVar m Bool
  -> m Token
  -> m Token
getTokenIOSimFixed now storedToken initLock refreshToken = do
  mToken <- MVar.tryReadMVar storedToken
  isInitialized <- MVar.readMVar initLock
  case mToken of
    Nothing | not isInitialized ->
      MVar.modifyMVar initLock $ \isInitialized' -> do
        if isInitialized'
          then do
            token <- MVar.readMVar storedToken
            pure (True, token)
          else do
            token <- refreshToken
            MVar.putMVar storedToken token
            pure (True, token)
    Nothing ->
      MVar.readMVar storedToken
    Just token ->
      case checkExpiry now token of
        IsOk -> pure token
        IsExpired ->
          MVar.modifyMVar storedToken $ \oaToken ->
            case checkExpiry now oaToken of
              IsOk -> pure (oaToken, oaToken)
              IsExpired -> do
                newToken <- refreshToken
                pure (newToken, newToken)
