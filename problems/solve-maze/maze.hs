-- | Solution idea got from: https://www.cs.bu.edu/teaching/alg/maze/
--
module Main where

import System.Environment
import Data.Monoid

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Maze = Maze { unMaze :: [MRow] }
type MRow = [Node]
data Node = PASS | BLOCK | PATH | NOACCESS | GOAL
            deriving (Eq, Enum)

type Row = Int
type Column = Int
type Point = (Row, Column)

instance Show Node where
  show PASS     = "-"
  show BLOCK    = "#"
  show PATH     = "+"
  show NOACCESS = "!"
  show GOAL     = "@"


-- | Failed to leverage Read class
--
toNode :: Char -> Node
toNode '-' = PASS
toNode '#' = BLOCK
toNode '@'   = GOAL
toNode _   = NOACCESS

--------------------------------------------------------------------------------

isOutside :: Maze     -- ^ Bounded index
           -> Point   -- ^ A certain position
           -> Bool
isOutside m (r, c) = let (rows, cols) = mazeSize m in
                     r < 1 || r > rows || c < 1 || c > cols

isGoal :: Maze -> Point -> Bool
isGoal m p = GOAL == mazeNode m p

isOpen :: Maze -> Point -> Bool
isOpen m p = PASS == mazeNode m p

north, south, east, west :: Point -> Point
north (r, c) = (r-1, c)
south (r, c) = (r+1, c)
east  (r, c) = (r, c+1)
west  (r, c) = (r, c-1)

mazeSize :: Maze -> (Int, Int)
mazeSize (Maze ass) = let rows = length ass
                          cols = length (ass !! 1) in
                      (rows, cols)

mazeNode :: Maze -> Point -> Node
mazeNode m (r, c) = unMaze m !! (r-1) !! (c-1)


mkMaze :: [[Node]] -> Maze
mkMaze = Maze


findPath :: Maze -> ([Point], [Point])
findPath m = findPath2 m (1,1)

findPath2 :: Maze -> Point -> ([Point], [Point])
findPath2 m p =
  let (result, accessed) = findPath' m p [] in
  case result of
    [] -> ([], accessed)
    _ -> (result, accessed `diffP` result)

-- | Record all not accessiable nodes
--
diffP :: [Point]         -- ^ All PASS nodes has been visited
         -> [Point]      -- ^ All PATH nodes
         -> [Point]
diffP xss yss = filter ep xss
                where ep p = p `notElem` yss

-- | Find the routes Points
--
findPath' :: Maze                   -- ^ The Maze
             -> Point               -- ^ Start point
             -> [Point]             -- ^ Points has been visited
             -> ([Point], [Point])  -- ^ A possible route and all visited nodes
findPath' m p ns
  | isOutside m p = ([], ns)
  | isGoal m p = (p: ns, ns)
  | not (isOpen m p) = ([], ns)
  | p `elem` ns = ([], ns)
  | otherwise = let nss = p : ns in
                findPath' m (north p) nss
                `mappend`
                findPath' m (east p) nss
                `mappend`
                findPath' m (south p) nss
                `mappend`
                findPath' m (west p) nss


-- | Find route in a maze and generate new Maze base on found route.
--   Will error out while no route found.
--
playMaze :: Maze
            -> Either Maze Maze     -- ^ A Maze has a successful route or not
playMaze m = let (paths, accesses) = findPath m
                 (rows, cols) = mazeSize m
                 nodes = [ [genNode (r, c) paths accesses | c <- [1..cols] ] | r <- [1..rows] ]
                 mz = mkMaze nodes in
             if null paths then Right mz else Left mz
             where genNode p path access
                     | isGoal m p      = GOAL       -- ^ Keep the GOAL position
                     | p `elem` path   = PATH
                     | p `elem` access = NOACCESS
                     | otherwise       = mazeNode m p


--------------------------------------------------------------------------------

printMaze :: Maze -> IO ()
printMaze (Maze rows) = let rowElems = map (unwords . map show) rows in
                        mapM_ print rowElems

play :: Maze -> IO ()
play m = do
  putStrLn "Start play maze:"
  printMaze m
  rm <- case playMaze m of
    Right r -> putStrLn "Uh oh, I could not find the treasure :-(" >> return r
    Left l ->  putStrLn "Get Relust:" >> return l
  printMaze rm

readMaze :: String        -- ^ Map file name
            -> IO Maze
readMaze f = do
  contents <- readFile f
  return $ mkMaze $ map (map toNode) $ lines contents

main :: IO ()
main = do
  args <- getArgs
  case args of
    (f:_) -> readMaze f >>= play
    []    -> play m2

--------------------------------------------------------------------------------
-- Sample Maze
--------------------------------------------------------------------------------

mazeElems :: [[Node]]
mazeElems = [ [PASS, PASS, PASS, BLOCK, PASS, PASS, BLOCK, BLOCK, BLOCK, PASS, PASS, BLOCK, PASS]
            , [PASS, BLOCK, PASS, PASS, PASS, BLOCK, PASS, PASS, PASS, PASS, BLOCK, BLOCK, PASS]
            , [BLOCK, BLOCK, BLOCK, BLOCK, PASS, BLOCK, PASS, BLOCK, PASS, BLOCK, PASS, BLOCK, BLOCK]
            , [PASS, PASS, PASS, BLOCK, PASS, PASS, PASS, BLOCK, PASS, BLOCK, PASS, PASS, PASS]
            , [PASS, BLOCK, PASS, BLOCK, BLOCK, BLOCK, BLOCK, PASS, PASS, PASS, BLOCK, BLOCK, PASS]
            , [PASS, BLOCK, PASS, PASS, PASS, PASS, PASS, PASS, BLOCK, PASS, PASS, PASS, PASS]
            , [PASS, BLOCK, BLOCK, BLOCK, BLOCK, BLOCK, BLOCK, BLOCK, BLOCK, BLOCK, BLOCK, BLOCK, BLOCK]
            , [PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, GOAL]]


m1 :: Maze
m1 = mkMaze mazeElems

m2 :: Maze
m2 = mkMaze [ [ PASS, PASS, BLOCK]
            , [ BLOCK, PASS, BLOCK]
            , [ BLOCK, PASS, GOAL]
            ]

m3 :: Maze
m3 = mkMaze [ [ PASS, PASS, BLOCK]
            , [ BLOCK, BLOCK, BLOCK]
            , [ BLOCK, PASS, GOAL]
            ]
