module MyEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where
    go (Right _) as = as
    go (Left a) as = a : as

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where
    go (Left _) as = as
    go (Right a) as = a : as

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where
    go (Left l) (ls, rs) = (l : ls, rs)
    go (Right r) (ls, rs) = (ls, r : rs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right r) = Just (f r)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left l) = f l
either' f g (Right r) = g r

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

main :: IO ()
main = do
  let input = [Right 1, Left 2, Left 3, Right 4]
  print $ lefts' input
  print $ rights' input
  print $ partitionEithers' input
  print $ eitherMaybe' (+ 2) (Left 3)
  print $ eitherMaybe' (+ 2) (Right 3)
  print $ either' id (\x -> x - 4) (Right 10)
  print $ either' (+ (3 :: Int)) id (Left 10)
