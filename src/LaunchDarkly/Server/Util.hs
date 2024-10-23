module LaunchDarkly.Server.Util
    ( fst3
    , snd3
    , trd
    )
where

-- |
-- Returns the first element of a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- |
-- Returns the second element of a 3-tuple.
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- |
-- Returns the third element of a 3-tuple.
trd :: (a, b, c) -> c
trd (_, _, x) = x
