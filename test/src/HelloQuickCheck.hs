module HelloQuickCheck where

import Data.Char
import Test.QuickCheck

--instance Arbitrary Char where
--    arbitrary     = choose ('\32', '\128')
--    coarbitrary c = variant (ord c `rem` 4)


getList :: IO String
getList = fmap take5 getContents

-- | quickCheck test
-- @
-- quickCheck (\s -> length (take5 s) <= 5)
-- @
-- 
take5 :: String -> String
take5 = take 5 . filter (`elem` ['a'..'e'])
