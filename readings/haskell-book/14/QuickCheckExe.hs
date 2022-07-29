module QuickCheckExe where

import Data.List
import Data.Char
import Test.QuickCheck

genNat :: Gen Integer
genNat = suchThat (chooseAny :: Gen Integer) (\x -> x > 0)

genSmallNat :: Gen Int
genSmallNat = elements [1 .. 5]

{- half -}
half :: Fractional a => a -> a
half x = x / 2

prop_half :: Float -> Bool
prop_half x = half x * 2 == x

{- sort -}
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)

prop_listOrdered1 :: [Int] -> Bool
prop_listOrdered1 = listOrdered . sort

prop_listOrdered2 :: [String] -> Bool
prop_listOrdered2 = listOrdered . sort

{- square -}
square :: Num a => a -> a
square x = x * x

prop_squareInteger :: Integer -> Bool
prop_squareInteger x = square x == x * x

prop_squareFloat :: Float -> Bool
prop_squareFloat x = square x == x * x

{- addition -}
prop_plusAssociativeInt :: Integer -> Integer -> Integer -> Bool
prop_plusAssociativeInt x y z = x + (y + z) == (x + y) + z

prop_plusAssociativeFloat :: Float -> Float -> Float -> Bool
prop_plusAssociativeFloat x y z = x + (y + z) == (x + y) + z

prop_plusCommutativeDouble :: Double -> Double -> Bool
prop_plusCommutativeDouble x y = x + y == y + x

prop_plusCommutativeFloat :: Float -> Float -> Bool
prop_plusCommutativeFloat x y = x + y == y + x

-- sqrt :: Floating a => a -> a
squareIdentify :: Float -> Float
squareIdentify = square . sqrt

-- | doesn't hold
prop_squareIdentify :: Float -> Bool
prop_squareIdentify x
  | x <= 0 = True
  | otherwise = squareIdentify x == x

prop_quotRem :: Property
prop_quotRem =
  forAll
    genNat
    ( \x ->
        forAll
          genNat
          (\y -> (quot x y) * y + (rem x y) == x)
    )

prop_divMod :: Property
prop_divMod =
  forAll
    genNat
    ( \x ->
        forAll
          genNat
          (\y -> (div x y) * y + (mod x y) == x)
    )

prop_powerAsso :: Property
prop_powerAsso =
  forAll
    genSmallNat
    ( \x ->
        forAll
          genSmallNat
          ( \y ->
              forAll
                genSmallNat
                (\z -> (x ^ y) ^ z == x ^ (y ^ z))
          )
    )

prop_reverse :: [Char] -> Bool
prop_reverse xs = xs == reverse (reverse xs)

-- TODO: how to write this??
-- prop_compose ::

prop_cons :: [Char] -> [Char] -> Bool
prop_cons xs ys = foldr (:) xs ys == xs ++ ys

prop_concat :: [ [Char] ] -> Bool
prop_concat xss = foldr (++) [] xss == concat xss

genListAndCount :: Gen ([Int], Int)
genListAndCount = do
  xs <- listOf1 (arbitrary :: Gen Int)
  n <- elements [1..length xs]
  return (xs, n)

prop_len :: Property
prop_len =
  forAll genListAndCount
  (\(xs, n) -> length (take n xs) == n)

prop_readShow :: Integer -> Bool
prop_readShow x = (read $ show x) == x
prop_readShow2 :: Double -> Bool
prop_readShow2 x = (read $ show x) == x
prop_readShow3 :: Char -> Bool
prop_readShow3 x = (read $ show x) == x

{- Idempotence -}
capWord :: String -> String
capWord [] = []
capWord (x:xs) = toUpper x : xs

twice :: (a -> a) -> a -> a
twice f = f . f
fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

prop_twice :: String -> Bool
prop_twice xs =
  (capWord xs == twice capWord xs)
  &&
  (capWord xs == fourTimes capWord xs)

prop_twice2 :: [Integer] -> Bool
prop_twice2 xs =
  (sort xs == twice sort xs)
  &&
  (sort xs == fourTimes sort xs)

main :: IO ()
main = do
  putStr "prop_half: " >> quickCheck prop_half
  putStr "prop_listOrdered1: " >> quickCheck prop_listOrdered1
  putStr "prop_listOrdered2: " >> quickCheck prop_listOrdered2
  putStr "prop_squareIdentity: " >> quickCheck prop_squareIdentify
  putStr "prop_plusAssociativeFloat: " >> quickCheck prop_plusAssociativeFloat
  putStr "prop_plusAssociativeInt: " >> quickCheck prop_plusAssociativeInt
  putStr "prop_plusCommutativeDouble: " >> quickCheck prop_plusCommutativeDouble
  putStr "prop_plusCommutativeFloat: " >> quickCheck prop_plusCommutativeFloat
  putStr "prop_quotRem: " >> quickCheck prop_quotRem
  putStr "prop_divMod: " >> quickCheck prop_divMod
  putStr "prop_powerAsso: " >> quickCheck prop_powerAsso
  putStr "prop_reverse: " >> quickCheck prop_reverse
  putStr "prop_cons: " >> quickCheck prop_cons
  putStr "prop_concat: " >> quickCheck prop_concat
  putStr "prop_len: " >> quickCheck prop_len
  putStr "prop_readShow: " >> quickCheck prop_readShow >> quickCheck prop_readShow2 >> quickCheck prop_readShow3
  putStr "prop_twice: " >> quickCheck prop_twice >> quickCheck prop_twice2
