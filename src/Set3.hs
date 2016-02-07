{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

-------------------------
-- Set 3: Combinations --
-------------------------

-- Generating combinations

allPairs' :: [a] -> [b] -> [(a,b)]
allPairs' as bs = foldr (\a xs -> foldr (\b ys -> [(a, b)] ++ ys) xs bs) [] as

-- Poker hands

data Card = Card Int String
instance Show Card where
    show (Card rank suit) = (show rank) ++ suit

allCards' :: [Int] -> [String] -> [Card]
allCards' ranks suits = map (\pair -> Card (fst pair) (snd pair)) (allPairs ranks suits)

-- Generalizing pairs and cards

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' func as bs = foldr (\a xs -> foldr (\b ys -> [func a b] ++ ys) xs bs) [] as

allPairs :: [a] -> [b] -> [(a,b)]
allPairs as bs = allCombs (\a b -> (a, b)) as bs

allCards :: [Int] -> [String] -> [Card]
allCards ranks suits = allCombs (\rank suit -> Card rank suit) ranks suits

-- Combinations of three things

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' func as bs cs = foldr (\a xs -> foldr (\b ys -> foldr (\c zs -> [func a b c] ++ zs) ys cs) xs bs) [] as

-- Combinations of more things

combStep :: [a -> b] -> [a] -> [b]
combStep funcs as = foldr (\f xs -> foldr (\a ys -> [f a] ++ ys) xs as) [] funcs

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs func as bs = combStep (map func as) bs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 func as bs cs = combStep (combStep (map func as) bs) cs

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 func as bs cs ds = combStep (combStep (combStep (map func as) bs) cs) ds
