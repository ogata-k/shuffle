module Data.Random.Shuffle(
  shuffle
, autoShuffle
)where

import System.Random
import Data.List (nub, sortOn)
import Control.Applicative ((<$>), (<*>))

getNubRandomRs :: RandomGen g => g -> (Int, Int) -> [Int]
getNubRandomRs g (l, h) = take (abs (h - l) + 1) $ nub $ randomRs (l, h) g

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle _ [] = []
shuffle g xs = map fst $ sortOn snd $ zip xs rs
    where
        rs = getNubRandomRs g (1, length xs)

autoShuffle ::[a] -> IO [a]
autoShuffle [] = pure []
autoShuffle xs = shuffle <$> newStdGen <*> (pure xs)
