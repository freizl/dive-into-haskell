-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 8/9 November

import System.Random
import Data.List

-- Importing the keymap module

--
-- DIFF
-- When List solution, readDB and getSample is fast but get is slow.
-- When Tree solution, readDB and getSample is slow but get is fast.
--
--import KeymapList
import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum [ length x | (_, (x,_)) <- xs]

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (b, (p, u)) = b ++ dots b ++ p ++ dots p ++ dots u ++ u
                           where dots str
                                   | length str >= n = ""
                                   | otherwise = replicate (n - length str) '.'

showCatalogue :: Catalogue -> String
showCatalogue cs = let xs = toList cs
                       l = longestProductLen xs
                       ys = map (formatLine l) xs
                       lb = "\n"
                       str = map (\x -> x ++ lb) ys
                   in
                   concat str

-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]
               where isJust Nothing = False
                     isJust (Just _) = True

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems bs cs = [ p | (b, p) <- toList cs, b `elem` bs ]





-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
