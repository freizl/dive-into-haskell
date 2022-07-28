module MyMaybe where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

t1 :: IO ()
t1 = do
  print $ isJust (Just 1)
  print $ isJust Nothing
  print $ isNothing (Just 1)
  print $ isNothing Nothing

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe def _ Nothing = def
myMaybe _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe a = myMaybe a id

t2 :: IO ()
t2 = do
  print $ myMaybe 0 (+ 1) (Just 1)
  print $ myMaybe 0 (+ 1) Nothing
  print $ fromMaybe 0 (Just 1)
  print $ fromMaybe 0 Nothing

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

t3 :: IO ()
t3 = do
  print $ listToMaybe [1, 2, 3]
  print (listToMaybe [] :: Maybe Int)
  print $ maybeToList (Just 2)
  print (maybeToList Nothing :: [Int])

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes ((Just x) : xs) = x : catMaybes xs

catMaybes2 :: [Maybe a] -> [a]
catMaybes2 = concat . map maybeToList

t4 :: IO ()
t4 = do
  print $ catMaybes [Just 1, Nothing, Just 2]
  print (catMaybes (take 3 $ repeat Nothing) :: [Int])
  print $ catMaybes2 [Just 1, Nothing, Just 2]
  print (catMaybes2 (take 3 $ repeat Nothing) :: [Int])

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f2 (Just [])
  where
    f1 :: Maybe a -> Maybe [a] -> Maybe [a]
    f1 a b = case a of
      Nothing -> Nothing
      Just x -> case b of
        Nothing -> Nothing
        Just y -> Just (x : y)
    f2 :: Maybe a -> Maybe [a] -> Maybe [a]
    f2 a b = do
      x <- a
      y <- b
      return (x : y)

t5 :: IO ()
t5 = do
  print $ flipMaybe [Just 1, Just 2, Just 3]
  print $ flipMaybe [Just 1, Nothing, Just 3]
