-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (27/28 Oct)

import List (nub)
import Data.List
import Char
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
           ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:phil.scott@ed.ac.uk\">Phil Scott</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br><b>TA:</b> "
            , "mailto:phil.scott@ed.ac.uk\">Phil Scott</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Philip Wadler","wadler@inf.ed.ac.uk")
               , ("Phil Scott","phil.scott@ed.ac.uk")]

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
sameString str1 str2 = map (toLower) str1 == map (toLower) str2

--Doesn't work!
prop_sameString :: String -> Bool
prop_sameString str  = 
    map toLower str `sameString` map toUpper str

-- 2.
prefix :: String -> String -> Bool
prefix sub str = map toLower sub `isPrefixOf` map toLower str

-- Works sometimes
prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr (map toLower str) &&
		      prefix substr (map toUpper str)
                          where
                            substr  =  take n str


-- 3.
contains :: String -> String -> Bool
contains sub str = map toLower sub `isInfixOf` map toLower str

-- Doesn't work! 
prop_contains :: String -> Int -> Int -> Bool
prop_contains str x y = contains sub (map toLower str) &&
		      contains sub (map toUpper str)
                          where
                            sub  =  take x str


-- 4.
takeUntil :: String -> String -> String
takeUntil _ [] = []
takeUntil sub str
	| sub `prefix` str			 = []
	| not (sub `prefix` str) = (head str) : takeUntil sub (tail str)

dropUntil :: String -> String -> String
dropUntil _ [] = []
dropUntil sub str
	| sub `prefix` str			 = drop (length sub) str
	| not (sub `prefix` str) = dropUntil sub (tail str)


-- 5.
split :: String -> String -> [String]
split sep [] = []
split sep str = takeUntil sep str : split sep (dropUntil sep str)

reconstruct :: String -> [String] -> String
reconstruct sep [str] = str
reconstruct sep (str:strs) = str ++ sep ++ (reconstruct sep strs)

-- Doesn't Work!
prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML html = tail [ link | link <- (split "<a href=\"" html) ] 

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [ link | link <- links, "mailto:" `prefix` link ] 


-- 8.
link2pair :: Link -> (Name, Email)
link2pair link = ( takeUntil "</a>" (dropUntil "\">" link), (takeUntil "\">" (dropUntil "mailto:" link) ) )


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = nub [ link2pair mail | mail <- ( takeEmails (linksFromHTML html) ) ]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name list = [ (x,y) | (x,y) <- list, name `contains` x ]


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name (emailsFromHTML html)


-- Optional Material

-- 12.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]