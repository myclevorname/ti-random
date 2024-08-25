{-|
 -Module      : Data.Random.TI
  Description : Implemenation of the TI-83+ RNG
  Copyright   : (c) Samuel Connelly, 2024

  License     : MIT
  Maintainer  : myclevorname@gmail.com
  Stability   : Stable
  Portability : All

  This module provides an implementation of the TI-83 Plus TI-BASIC random number
  generator. The random number generators themselves produce a pair containing
  the result and the random seed after the operation occurs.
 - -}

module Data.Random.TI (RandomSeed, rand, randInt, randIntList, initialSeed, makeSeed, randIntNoRep) where
--import Data.Int (Int64)
import Data.Ratio
import Data.List (elemIndex)

-- |The random seed as used by the random number generator. Typically, you'd want to use makeSeed.
data RandomSeed = Seed Int Int
    deriving Show

modulus1, modulus2, multiplier1, multiplier2 :: Int

modulus1 = 2147483563
modulus2 = 2147483399
multiplier1 = 40014
multiplier2 = 40692

-- |The initial seed used by the random number generator.
initialSeed :: RandomSeed

initialSeed = Seed 12345 67890

-- |Generates a random rational number between 0 and 1.
rand :: RandomSeed -> (Ratio Int, RandomSeed)

rand (Seed seed1 seed2) = (result, Seed newSeed1 newSeed2)
    where
    newSeed1 = (seed1 * multiplier1) `mod` modulus1
    newSeed2 = (seed2 * multiplier2) `mod` modulus2

    result1 = fromIntegral (newSeed1 - newSeed2) % modulus1
    result = if result1 < 0 then result1+1 else result1

--         min    max
-- |Generates a random integer between the first argument and second argument inclusively.
randInt :: Int -> Int -> RandomSeed -> (Int, RandomSeed)

randInt minValue maxValue origSeed = (result, newSeed)
    where
    (x, newSeed) = rand origSeed
    --                     The result can include maxValue and minValue
    --                     Even if the random rational number happens to be >= 1, that isn't *too* bad.
    result = floor (x * (fromIntegral $ maxValue-minValue+1)) + minValue

-- }Generates a list of random integers between the first argument and second argument inclusively.
randIntList :: Int -> Int -> Int -> RandomSeed -> ([Int], RandomSeed)
randIntList _ _ 0 seed = ([], seed)
randIntList minValue maxValue count seed = (s:xs, finalSeed)
    where
    (s, nextSeed) = randInt minValue maxValue seed
    (xs, finalSeed) = randIntList minValue maxValue (count-1) nextSeed

-- |Generates a randok seed. Negative numbers map to their positive counterparts, and 0 returns initialSeed.
makeSeed :: Int -> RandomSeed

makeSeed n = case signum n of
    0  -> initialSeed
    -1 -> makeSeed (-n)
    1  -> Seed ((multiplier1 * n) `mod` modulus1) (n `mod` modulus2)
    _  -> undefined

-- |Generates a list of random integers between the first argument and second argument inclusively. If the third argument is Nothing, then the function returns a random shuffle of [minValue..maxValue].
randIntNoRep :: Int -> Int -> (Maybe Int) -> RandomSeed -> ([Int], RandomSeed)
randIntNoRep _ _ (Just 0) seed = ([], seed)
randIntNoRep minValue maxValue Nothing seed = loop [minValue..maxValue] [] seed
    where
    loop xs result currentSeed = loop (dropElem (xs!!x) xs) (result ++ [xs!!x]) newSeed
        where
        (x, newSeed) = (randInt 0 (length xs-1) currentSeed)
randIntNoRep minValue maxValue (Just a) seed = loop [] a seed False
    where
    loop xs count currentSeed inc = case idx of
        Just i  -> loop (xs ++ [attempts!!i]) (count-1) (snd $ randIntList 0 1 (i+1) currentSeed) inc
        Nothing -> loop (xs ++ [head $ filter (flip notElem xs) (if inc then [minValue..maxValue] else [maxValue,maxValue-1..minValue])]) (count-1) finalAttemptSeed (not inc)
        where
        (attempts, finalAttemptSeed) = randIntList minValue maxValue 3 currentSeed
        idx = if any (flip elem xs) attempts then
            elemIndex (head $ filter (flip elem xs) attempts) attempts else
            Nothing

dropElem :: (Eq a) => a -> [a] -> [a]
dropElem = filter . (/=)
