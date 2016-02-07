{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

---------------------------------
-- Set 2: Failing Computations --
---------------------------------

-- The Maybe Type

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

-- Build a library of things that can fail

headMay :: [a] -> Maybe a
headMay []      = Nothing
headMay (x : _) = Just x

tailMay :: [a] -> Maybe [a]
tailMay []         = Nothing
tailMay (_ : tail) = Just tail

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ []       = Nothing
lookupMay p (x : xs) =
    if (fst x) == p then
        Just (snd x)
    else
        lookupMay p xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay n d = Just (n / d)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []       = Nothing
maximumMay (x : xs) = Just (foldr max x xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []       = Nothing
minimumMay (x : xs) = Just (foldr min x xs)

-- Chains of Failing Computations

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greek str =
    case (lookupMay str greek) of
        Nothing -> Nothing
        Just xs -> case (tailMay xs) of
            Nothing -> Nothing
            Just tail -> case (maximumMay tail) of
                Nothing -> Nothing
                Just max -> case (headMay xs) of
                    Nothing -> Nothing
                    Just head -> case (divMay (fromIntegral max) (fromIntegral head)) of
                        Nothing -> Nothing
                        Just quot -> Just quot

-- Generalizing chains of failures

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f a = case (a) of
    Nothing -> Nothing
    Just a  -> f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link a f = case (a) of
    Nothing -> Nothing
    Just a  -> f a

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greek str = divMayMaybe max head
    where xs   = lookupMay str greek
          tail = link xs tailMay
          head = link xs headMay
          max  = link tail maximumMay
          divMayMaybe a b = link a (\n -> link b (\d -> divMay (fromIntegral n) (fromIntegral d)))

-- Chaining variations

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries name1 name2 = sumMay salary1 salary2
    where salary1 = lookupMay name1 salaries
          salary2 = lookupMay name2 salaries
          sumMay x y = case x of
                            Nothing -> Nothing
                            Just a -> case y of
                                Nothing -> Nothing
                                Just b -> Just (a + b)

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink func maybeA maybeB = link maybeA (\a -> link maybeB (\b -> Just (func a b)))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 salaries name1 name2 = yLink (+) salary1 salary2
    where salary1 = lookupMay name1 salaries
          salary2 = lookupMay name2 salaries

mkMaybe :: a -> Maybe a
mkMaybe a = Just a

yLink' :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink' func maybeA maybeB = link maybeA (\a -> link maybeB (\b -> mkMaybe (func a b)))

addSalaries' :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries' salaries name1 name2 = sumMay salary1 salary2
    where salary1 = lookupMay name1 salaries
          salary2 = lookupMay name2 salaries
          sumMay x y = case x of
                            Nothing -> Nothing
                            Just a -> case y of
                                Nothing -> Nothing
                                Just b -> mkMaybe (a + b)

-- Tailprod

tailProd :: Num a => [a] -> Maybe a
tailProd xs = link (tailMay xs) (\tail -> mkMaybe (product tail))

tailSum :: Num a => [a] -> Maybe a
tailSum xs = link (tailMay xs) (\tail -> mkMaybe (sum tail))

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe func mayA = link mayA (\a -> mkMaybe (func a))

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 xs = transMaybe product (tailMay xs)

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 xs = transMaybe sum (tailMay xs)

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = transMaybe maximumMay (tailMay xs)

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin xs = transMaybe minimumMay (tailMay xs)

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing  = Nothing
combine (Just m) = m

tailMax2 :: Ord a => [a] -> Maybe a
tailMax2 xs =  combine (transMaybe maximumMay (tailMay xs))

tailMin2 :: Ord a => [a] -> Maybe a
tailMin2 xs = combine (transMaybe minimumMay (tailMay xs))
