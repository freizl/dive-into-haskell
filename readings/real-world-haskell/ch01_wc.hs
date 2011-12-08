-- file: chp01/WC.hs

main = interact worldCount
  where worldCount input = show (length (words input)) ++ "\n"

--  where worldCount input = show (length (lines input)) ++ "\n"
