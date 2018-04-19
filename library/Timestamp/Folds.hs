{-|
Folds on timestamped values
-}
module Timestamp.Folds
where

import Timestamp.Prelude hiding (maximum)
import Timestamp
import Control.Foldl


untimestamped :: Fold input output -> Fold (Timestamped input) output
untimestamped =
  premap (\ (Timestamped _ x) -> x)

filteringByUtcTime :: (UTCTime -> Bool) -> Fold (Timestamped input) output -> Fold (Timestamped input) output
filteringByUtcTime filter =
  filteringByTimestamp (filter . timestampUtcTime)

filteringByTimestamp :: (Timestamp -> Bool) -> Fold (Timestamped input) output -> Fold (Timestamped input) output
filteringByTimestamp filter =
  prefilter (\ (Timestamped ts _) -> filter ts)

inUtcTimeRange :: UTCTime -> UTCTime -> Fold (Timestamped input) output -> Fold (Timestamped input) output
inUtcTimeRange start end =
  inTimestampRange (utcTimeTimestamp start) (utcTimeTimestamp end)

inTimestampRange :: Timestamp -> Timestamp -> Fold (Timestamped input) output -> Fold (Timestamped input) output
inTimestampRange start end =
  filteringByTimestamp (\ ts -> ts >= start && ts < end)
