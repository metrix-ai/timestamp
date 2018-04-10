module Timestamp.Data.Conversion
where

import Timestamp.Prelude
import Timestamp.Data.TypesAndInstances


timestampMicroSecondsInt64 :: Timestamp -> Int64
timestampMicroSecondsInt64 (Timestamp prim) = prim

timestampNominalDiffTime :: Timestamp -> NominalDiffTime
timestampNominalDiffTime =
  unsafeCoerce . (* 1000000) . toInteger . timestampMicroSecondsInt64

timestampPosixTime :: Timestamp -> POSIXTime
timestampPosixTime = timestampNominalDiffTime

timestampUtcTime :: Timestamp -> UTCTime
timestampUtcTime = posixSecondsToUTCTime . timestampPosixTime

timestampDay :: Timestamp -> Day
timestampDay = utctDay . timestampUtcTime

utcTimeTimestamp :: UTCTime -> Timestamp
utcTimeTimestamp =
  posixTimeTimestamp . utcTimeToPOSIXSeconds

posixTimeTimestamp :: POSIXTime -> Timestamp
posixTimeTimestamp =
  Timestamp . fromIntegral . flip div 1000000 . (unsafeCoerce :: NominalDiffTime -> Integer)
