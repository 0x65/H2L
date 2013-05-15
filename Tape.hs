module Tape (
      Tape
    , makeTape
    , readTape
    , writeTape
    , updateTape
    , moveTapeLeft
    , moveTapeRight
) where

-- | A data type for infinite tapes
data Tape a = TapeI ([a], [a]) a
    deriving (Show)

-- | Constructs a tape from a (possibly empty) list of
--   initial values and a default or "blank" value
makeTape :: [a] -> a -> Tape a
makeTape initial def = TapeI ([], initial) def

-- | Reads the tape at the current head
readTape :: Tape a -> a
readTape (TapeI (_, []) d) = d
readTape (TapeI (_, x:xs) _) = x

-- | Writes a value to the tape at the current head
writeTape :: Tape a -> a -> Tape a
writeTape (TapeI ([], []) d) v = TapeI ([], [v]) d
writeTape (TapeI (l, x:xs) d) v = TapeI (l, v:xs) d
writeTape (TapeI (x:xs, []) d) v = TapeI (v:xs, []) d

-- | Utility function to combine read and write actions
updateTape :: Tape a -> (a -> a) -> Tape a
updateTape t f = writeTape t (f (readTape t))

-- | Moves the tape left by one cell
moveTapeLeft :: Tape a -> Tape a
moveTapeLeft (TapeI ([], r) d) = TapeI ([], d:r) d
moveTapeLeft (TapeI (x:xs, r) d) = TapeI (xs, x:r) d

-- | Moves the tape right by one cell
moveTapeRight :: Tape a -> Tape a
moveTapeRight (TapeI (l, []) d) = TapeI (d:l, []) d
moveTapeRight (TapeI (l, x:xs) d) = TapeI (x:l, xs) d
