-- Informatics 1 - Functional Programming
-- Tutorial 4
--
-- Due: the tutorial of week 6 (25/26 Oct)

import Data.List
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

tutorialURL = "http://www.inf.ed.ac.uk/admin/itodb/mgroups/tuts/inf1-fp.html"
groupURL    = "http://www.inf.ed.ac.uk/admin/itodb/mgroups/stus/inf1-fp.html"
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
     let emails = emailsFromHTML html
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = emailsByNameFromHTML html name
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString xs ys = map toUpper xs == map toUpper ys

prop_sameString :: String -> Bool
prop_sameString str  =
    map toLower str `sameString` map toUpper str

-- 2.
prefix :: String -> String -> Bool
prefix xs ys = map toUpper xs `isPrefixOf` map toUpper ys

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr (map toLower str) &&
		      prefix substr (map toUpper str)
                          where
                            substr  =  take n str


-- 3.
contains :: String -> String -> Bool
contains [] [] = True
contains _ [] = False
contains substr str = substr `prefix` str
                      || contains (tail str) substr


prop_contains :: String -> Int -> Int -> Bool
prop_contains xs i j = contains substr (map toLower xs)
                       && contains substr (map toUpper xs)
                       where substr = drop i (take j xs)


-- 4.
takeUntil :: String -> String -> String
takeUntil substr str
  | not (substr `contains` str) = str
  | substr `prefix` str = []
  | otherwise = head str : takeUntil substr (tail str)

dropUntil :: String -> String -> String
dropUntil substr str
  | not (substr `contains` str) = []
  | substr `prefix` str = drop (length substr) str
  | otherwise = dropUntil substr (tail str)


-- 5.
split :: String -> String -> [String]
split [] _ = error "invalid input of split function"
split _ [] = []
split sep str
  | not (sep `contains` str) = [str]
  | otherwise = takeUntil sep str : split sep (dropUntil sep str)

reconstruct :: String -> [String] -> String
reconstruct _ [] = []
reconstruct _ (x:[]) = x
reconstruct sep (x:xs) = x ++ sep ++ reconstruct sep xs

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML = tail . split "<a href=\""

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails = filter isMailLink

isMailLink :: String -> Bool
isMailLink = prefix "mailto"

-- 8.
link2pair :: Link -> (Name, Email)
link2pair xs
  | not (contains "mailto:" xs) = error ("invalid link: " ++ xs)
  | otherwise = let x1 = dropUntil "mailto:" xs
                    email = takeUntil "\">" x1
                    x2 = dropUntil "\">" x1
                    name = takeUntil "</a>" x2
                in
                (name, email)


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML = map link2pair . takeEmails . linksFromHTML

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name xs = [ p | p@(n, e) <- xs, name `contains` n ]


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name (emailsFromHTML html)


-- Optional Material

-- 12.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
