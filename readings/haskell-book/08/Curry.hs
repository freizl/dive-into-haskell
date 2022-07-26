-- | 

module Curry where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- "woops mrow woohoo!"
t1 = appedCatty "woohoo!"

-- "1 mrow haha"
t2 = frappe "1"

-- "woops mrow 2 mrow haha"
t3 = frappe (appedCatty "2")

-- "woops mrow blue mrow haha"
t4 = appedCatty (frappe "blue")

-- "pink mrow haha mrow green mrow woops mrow blue"
t5 = cattyConny (frappe "pink")
                (cattyConny "green" (appedCatty "blue"))

-- "are mrow Pugs mrow awesome"
t6 = cattyConny (flippy "Pugs" "are") "awesome"

main :: IO ()
main = mapM_ putStrLn [t1, t2, t3, t4, t5, t6]
