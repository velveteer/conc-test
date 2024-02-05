module Common where

import qualified Data.Time as Time

data TokenExpiry = IsExpired | IsOk

type Token = (String, Time.UTCTime)

checkExpiry :: Time.UTCTime -> Token -> TokenExpiry
checkExpiry now (_, expiry) = do
  if Time.diffUTCTime expiry now < 300
    then IsExpired
    else IsOk
