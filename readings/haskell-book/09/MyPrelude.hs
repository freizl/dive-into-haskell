-- |

module MyPrelude where

import Test.QuickCheck

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a->Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) = x == y || myElem x ys

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs: xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs
-- squishMap f xs = squish $ map f xs
-- squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = error "myMaximumBy: empty list"
myMaximumBy f [x] = x
myMaximumBy f (x:xs) =
  let a1 = case length xs of
        -- will not be zero
        1 -> xs !! 0
        _ -> myMaximumBy f xs
      a2 = x
  in
    case f a1 a2 of
      GT -> a1
      EQ -> a1
      LT -> a2

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [] = error "myMinimumBy: empty list"
myMinimumBy f [x] = x
myMinimumBy f (x:xs) =
  let a1 = case length xs of
        -- will not be zero
        1 -> xs !! 0
        _ -> myMinimumBy f xs
      a2 = x
  in
    case f a1 a2 of
      LT -> a1
      GT -> a2
      EQ -> a2

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

main :: IO ()
main = do
  (quickCheck . withMaxSuccess 10000) prop_elem
  (quickCheck . withMaxSuccess 10000) prop_squish
  (quickCheck . withMaxSuccess 10000) prop_maximum
  (quickCheck . withMaxSuccess 10000) prop_minimum

prop_elem :: Char -> [Char] -> Bool
prop_elem x xs =
  ( x `elem` xs ) == ( x `myElem` xs )
  &&
  ( x `elem` xs ) == ( x `myElem2` xs )

prop_squish :: [[Integer]] -> Bool
prop_squish xss = squish xss == concat xss && squishAgain xss == concat xss

prop_maximum :: Property
prop_maximum =
  forAll genIntegers
  (\xs -> maximum xs == myMaximum xs)

prop_minimum :: Property
prop_minimum =
  forAll genIntegers
  (\xs -> minimum xs == myMinimum xs)

-- Empty list test case is safely enough to skip
-- until find out a way to "quickCheck" it
--
genIntegers :: Gen [Integer]
genIntegers = listOf1 chooseAny
