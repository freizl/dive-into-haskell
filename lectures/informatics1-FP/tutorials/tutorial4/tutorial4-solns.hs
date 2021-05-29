-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>

-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:c.banks@ed.ac.uk\">Chris Banks</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:c.banks@ed.ac.uk\">Chris Banks</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Chris Banks","c.banks@ed.ac.uk")]

-- </sample data>

-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>


-- 1.
sameString :: String -> String -> Bool
sameString str1 str2 = map toUpper str1 == map toUpper str2

-- The following *should* also work; however, you may find 
-- that when you test with QuickCheck you find a *bug* in the 
-- definitions of the character set:   
-- toLower (toUpper '\181') /= toLower '\181'
--
-- sameString str1 str2 = map toLower str1 == map toLower str2

prop_sameString :: String -> Bool
prop_sameString str = map toLower str `sameString` map toUpper str

-- 2.
prefix :: String -> String -> Bool
prefix substr str = substr `sameString` take (length substr) str

-- prefix' substr str = map toUpper substr `isPrefixOf` map toUpper str

prop_prefix :: String -> Int -> Bool
prop_prefix str n = prefix substr (map toLower str) &&
		      prefix substr (map toUpper str)
                          where
                            substr = take n str

-- 3.
contains :: String -> String -> Bool
contains substr str = or [ prefix substr (drop i str) | i <- [0..length str - 1] ]

-- We would get much faster solution if we used the "tails" function in Data.List. 
-- Which can be defined as
-- tails :: [a] -> [[a]]
-- tails []     = [[]]
-- tails (x:xs) = (x:xs):tails xs

-- We then write
-- contains substr str = or [ prefix substr s | s <- tails str ] 

-- Compare this solution when str is a huge string such as (replicate 10000 'a')

-- Or we can use the following recursive solution.
-- contains :: String -> String -> Bool
-- contains [] []         = True
-- contains _ []          = False
-- contains substr (c:cs) = prefix substr (c:cs) || contains' substr cs

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n m = contains substr (map toUpper str) &&
                          contains substr (map toLower str)
                              where
                                substr = take n (drop m str)

-- 4.
takeUntil :: String -> String -> String
takeUntil substr [] = ""
takeUntil substr (c:cs) | prefix substr (c:cs) = ""
                        | otherwise = c : takeUntil substr cs

dropUntil :: String -> String -> String
dropUntil substr [] = ""
dropUntil substr str | prefix substr str = drop (length substr) str
                     | otherwise = dropUntil substr (tail str)

-- A List comprehension version
-- dropUntil :: String -> String -> String
-- dropUntil substr str = case [ s | s <- tails str, prefix substr s ] of
--     []  -> ""
--     s:_ -> drop (length substr) s

-- 5.
split :: String -> String -> [String]
split "" str  = error "Can't split on an empty string"
split sep str  
    | contains sep str = takeUntil sep str : split sep (dropUntil sep str)
    | otherwise        = [str]

reconstruct :: String -> [String] -> String
reconstruct _ []           = []
reconstruct _ [str]        = str
reconstruct sep (str:strs) = str ++ sep ++ reconstruct sep strs

-- Alternative using foldr1:
--
-- reconstruct sep = foldr1 f
--    where
--      f xs ys = xs ++ sep ++ ys
--
-- Alternative using concat:
-- 
-- reconstruct sep (str:strs) = str ++ concat (map (sep ++) strs)
-- 
-- Alternative using intersperse:
--
-- reconstruct sep strs | not (null strs) = concat (intersperse sep strs)

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML doc = tail (split "<a href=\"" doc)

testLinksFromHTML :: Bool
testLinksFromHTML = linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [link | link <- links, prefix "mailto:" link]

-- Alternative solution: 
--
-- takeEmails links = filter (prefix "mailto:") links


-- 8.
link2pair :: Link -> (Name, Email)
link2pair link | contains "mailto:" link = (name, email)
               | otherwise = error "link2pair: not a mail adress"
    where email = takeUntil "\">"  (dropUntil "mailto:" link)
          name  = takeUntil "</a>" (dropUntil "\">" link)


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = nub [link2pair link | link <- takeEmails (linksFromHTML html)]

-- Alternative solution:
--
-- emailsFromHTML = nub . map link2pair . takeEmails . linksFromHTML

testEmailsFromHTML :: Bool
testEmailsFromHTML = emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name addrs = [(n, e) | (n, e) <- addrs, name `contains` n]

-- Alternative solution:
--
-- findEmail name addrs = filter (contains name . fst) addrs


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name (emailsFromHTML html)


-- Optional Material

-- 12.
--ppAddrBook :: [(Name, Email)] -> String
--ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]

ppAddrBook addr = unlines [ take (max + 2) (formatName name ++ repeat ' ') ++ email 
                          | (name,email) <- addr ] 
  where max = maximum (map (length . fst) addr)

formatName name  
    | contains name "," = name
    | otherwise = dropUntil " " name ++ ", " ++ takeUntil " " name
