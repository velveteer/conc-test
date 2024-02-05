module TokenConc where

import qualified Control.Concurrent.Classy as Conc
import qualified Data.Time as Time

import Common

getTokenConc
  :: (Conc.MonadConc m)
  => Time.UTCTime
  -> Conc.MVar m Token
  -> Conc.MVar m ()
  -> m Token
  -> m Token
getTokenConc now storedToken initLock refreshToken = do
  mToken <- Conc.tryReadMVar storedToken
  case mToken of
    Nothing -> do
      Conc.putMVar initLock ()
      mToken' <- Conc.tryReadMVar storedToken
      case mToken' of
        Just mToken'' -> do
          _ <- Conc.tryTakeMVar initLock
          pure mToken''
        Nothing -> do
          token <- refreshToken
          Conc.putMVar storedToken token
          _ <- Conc.tryTakeMVar initLock
          pure token
    Just token ->
      case checkExpiry now token of
        IsOk -> pure token
        IsExpired ->
          Conc.modifyMVar storedToken $ \oaToken ->
            case checkExpiry now oaToken of
              IsOk -> pure (oaToken, oaToken)
              IsExpired -> do
                newToken <- refreshToken
                pure (newToken, newToken)

getTokenConcFixed
  :: (Conc.MonadConc m)
  => Time.UTCTime
  -> Conc.MVar m Token
  -> Conc.MVar m Bool
  -> m Token
  -> m Token
getTokenConcFixed now storedToken initLock refreshToken = do
  mToken <- Conc.tryReadMVar storedToken
  isInitialized <- Conc.readMVar initLock
  case mToken of
    Nothing | not isInitialized ->
      Conc.modifyMVar initLock $ \isInitialized' -> do
        if isInitialized'
          then do
            token <- Conc.readMVar storedToken
            pure (True, token)
          else do
            token <- refreshToken
            Conc.putMVar storedToken token
            pure (True, token)
    Nothing ->
      Conc.readMVar storedToken
    Just token ->
      case checkExpiry now token of
        IsOk -> pure token
        IsExpired ->
          Conc.modifyMVar storedToken $ \oaToken ->
            case checkExpiry now oaToken of
              IsOk -> pure (oaToken, oaToken)
              IsExpired -> do
                newToken <- refreshToken
                pure (newToken, newToken)
