module Data.Random.TI (RandomSeed, rand, randInt, randIntList, initialSeed, makeSeed, randIntNoRep) where
--import Data.Int (Int64)
import Data.Ratio
import Data.List (elemIndex)

data RandomSeed = Seed Int Int
    deriving Show

modulus1, modulus2, multiplier1, multiplier2 :: Int

modulus1 = 2147483563
modulus2 = 2147483399
multiplier1 = 40014
multiplier2 = 40692

initialSeed :: RandomSeed

initialSeed = Seed 12345 67890

rand :: RandomSeed -> (Ratio Int, RandomSeed)

rand (Seed seed1 seed2) = (result, Seed newSeed1 newSeed2)
    where
    newSeed1 = (seed1 * multiplier1) `mod` modulus1
    newSeed2 = (seed2 * multiplier2) `mod` modulus2

    result1 = fromIntegral (newSeed1 - newSeed2) % modulus1
    result = if result1 < 0 then result1+1 else result1

--         min    max
randInt :: Int -> Int -> RandomSeed -> (Int, RandomSeed)

randInt minValue maxValue origSeed = (result, newSeed)
    where
    (x, newSeed) = rand origSeed
    --                     The result can include maxValue and minValue
    --                     Even if the random rational number happens to be >= 1, that isn't *too* bad.
    result = floor (x * (fromIntegral $ maxValue-minValue+1)) + minValue

randIntList :: Int -> Int -> Int -> RandomSeed -> ([Int], RandomSeed)
randIntList _ _ 0 seed = ([], seed)
randIntList minValue maxValue count seed = (s:xs, finalSeed)
    where
    (s, nextSeed) = randInt minValue maxValue seed
    (xs, finalSeed) = randIntList minValue maxValue (count-1) nextSeed

makeSeed :: Int -> RandomSeed

makeSeed n = case signum n of
    0  -> initialSeed
    -1 -> makeSeed (-n)
    1  -> Seed ((multiplier1 * n) `mod` modulus1) (n `mod` modulus2)
    _  -> undefined

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
dropElem = filter . (==)
