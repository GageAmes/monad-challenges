{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

---------------------------
-- Set 1: Random Numbers --
---------------------------

-- Random Number Generation

fiverandsHelper :: [Integer] -> Seed -> [Integer]
fiverandsHelper xs seed =
    if length xs == 5 then
        xs
    else
        let (nextRand, newSeed) = rand seed in
            fiverandsHelper (nextRand : xs) newSeed

fiverands :: [Integer]
fiverands = fiverandsHelper [] (mkSeed 1)

-- Random Character Generation

randLetter :: Seed -> (Char, Seed)
randLetter seed =
    let (num, newSeed) = rand seed in
        (toLetter num, newSeed)

randString3Helper :: String -> Seed -> String
randString3Helper str seed =
    if length str == 3 then
        str
    else
        let (letter, newSeed) = randLetter seed in
            randString3Helper (str ++ [letter]) newSeed

randString3 :: String
randString3 = randString3Helper "" (mkSeed 1)

-- More Generators

type Gen x = Seed -> (x, Seed)

randEven :: Gen Integer
-- randEven seed = let (num, newSeed) = rand seed in (num * 2, newSeed)

randOdd :: Gen Integer
-- randOdd seed = let (evenNum, newSeed) = randEven seed in (evenNum + 1, newSeed)

randTen :: Gen Integer
-- randTen seed = let (doubleNum, newSeed) = randEven seed in (doubleNum * 5, newSeed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA transform baseGen = \seed -> let (nextNum, nextSeed) = baseGen seed in (transform nextNum, nextSeed)

randEven = generalA (\x -> x * 2) rand
randOdd = generalA (\x -> x + 1) randEven
randTen = generalA (\x -> x * 5) randEven

-- Generalizing Random Pairs

randPair :: Gen (Char, Integer)
randPair seed =
    let (char, charSeed) = randLetter seed in
        let (num, numSeed) = rand charSeed in
            ((char, num), numSeed)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair genA genB = \seed ->
    let (aVal, aSeed) = genA seed in
        let (bVal, bSeed) = genB aSeed in
            ((aVal, bVal), bSeed)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB transform genA genB = \seed ->
    let (aVal, aSeed) = genA seed in
        let (bVal, bSeed) = genB aSeed in
            (transform aVal bVal, bSeed)

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 genA genB = generalB (\x -> \y -> (x, y)) genA genB

repRandomHelper :: Gen a -> ([a], Seed) -> ([a], Seed)
repRandomHelper gen (vals, seed) =
    let (val, seed') = gen seed in
        ((vals ++ [val]), seed')

-- Generalizing Lists of Generators

repRandom :: [Gen a] -> Gen [a]
repRandom gens = \seed -> foldr repRandomHelper ([], seed) gens

-- Threading the random number state

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo gen func = \seed ->
    let (val, seed') = gen seed in
        func val seed'

mkGen :: a -> Gen a
mkGen val = \seed -> (val, seed)
