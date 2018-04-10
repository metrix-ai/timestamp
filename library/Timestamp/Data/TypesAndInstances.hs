module Timestamp.Data.TypesAndInstances
where

import Timestamp.Prelude
import qualified Generic.Random as A


{-|
A value paired with a timestamp.
-}
data Timestamped event = Timestamped {-# UNPACK #-} !Timestamp !event

{-|
Amount of microseconds since @1970-01-01 00:00.000000 UTC@.
The semantics are the same as of 'Data.Time.Clock.POSIX.POSIXTime'.
This datatype is used instead of 'UTCTime' or 'Data.Time.Clock.POSIX.POSIXTime'
for space-efficiency.
-}
newtype Timestamp = Timestamp Int64


deriving instance Eq a => Eq (Timestamped a)
deriving instance Ord a => Ord (Timestamped a)
deriving instance Show a => Show (Timestamped a)
deriving instance Functor Timestamped
deriving instance Generic (Timestamped a)
instance Arbitrary a => Arbitrary (Timestamped a) where arbitrary = A.genericArbitraryU
instance Hashable a => Hashable (Timestamped a)
instance Serialize a => Serialize (Timestamped a)

deriving instance Eq Timestamp
deriving instance Ord Timestamp
deriving instance Show Timestamp
deriving instance Generic Timestamp
instance Arbitrary Timestamp where arbitrary = A.genericArbitraryU
instance Hashable Timestamp
instance Serialize Timestamp
