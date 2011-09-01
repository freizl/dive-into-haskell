module HelloQuickCheck where

getList :: IO String
getList = fmap take5 getContents

-- | quickCheck test
-- @
-- quickCheck (\s -> length (take5 s) <= 5)
-- @
-- 
take5 :: String -> String
take5 = take 5 . filter (`elem` ['a'..'e'])
