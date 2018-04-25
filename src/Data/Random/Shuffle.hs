module Data.Random.Shuffle(
  shuffle
, autoShuffle
)where

import System.Random
import Data.List (nub, sortOn)
import Control.Applicative ((<$>), (<*>))

getNubRandomRs :: (Eq a, Enum a, RandomGen g) => g -> (a, a) -> [a]
getNubRandomRs g (l, h) = map toEnum $ take (abs (hInt - lInt) + 1) $ nub $ randomRs (lInt, hInt) g
    where
        lInt = fromEnum l
        hInt = fromEnum h

shuffle :: (Eq a, Enum a, RandomGen g) => g -> [a] -> [a]
shuffle _ [] = []
shuffle g xs = map fst $ sortOn snd $ zip xs rs
    where
        rs = getNubRandomRs g (1, length xs)

autoShuffle :: (Eq a, Enum a) => [a] -> IO [a]
autoShuffle [] = pure []
autoShuffle xs = shuffle <$> newStdGen <*> (pure xs)
