module Main where

type Solution = [Int]
type Column = Int
type Row = Int
type Pos = (Column, Row)

queens :: Int -> [Solution]
queens 0 = [[]]
queens m = [ p ++ [n] | p <- prevs, n <- [1..8], safe p n ]
           where prevs = queens (m-1)

-- | Given a Solution over MxM board, is it safe to place a new one
--   to generate a (M+1)x(M+1) board.
--
safe :: Solution -> Int -> Bool
safe p n = and [ not $ check (i, j) (len+1, n) | (i, j) <- zip [1..len ] p ]
           where len = length p

-- | Whether two queens are checked each other
--
check :: Pos -> Pos -> Bool
check (i, j) (k, l) = j == l || (i + j) == (k + l) || (i-j) == (k-l)

-- | preety print the board
-- TODO: showing in wrong order (compare the a Solution with string output)
--
showS :: Solution -> [String]
showS p = foldr fix (emptyBoard len) $ zip [1..len] p
          where len = length p

pp :: [String] -> IO ()
pp ss = mapM_ print $ map spaces ss
   where spaces xs = concat [ "  "++[x] | x <- xs]


fix :: Pos -> [String] -> [String]
fix (i, j) board = take (j-1) board ++ rep i (board !! (j-1)) ++ drop j board
rep i row = [take i row ++ "X" ++ drop (i+1) row]

emptyBoard :: Int -> [String]
emptyBoard n = [ show j ++ replicate n '*' | j <- [n,n-1..1] ]
               ++ [' ' : concat [ show i | i <- [1..n] ] ]
sep :: IO ()
sep = print $ replicate 35 '='

-- main = pp . showS $ head $ queens 8
main = mapM_ ((>> sep) . pp . showS) (take 10 $ queens 8)
