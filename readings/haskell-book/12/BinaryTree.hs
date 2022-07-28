module BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
  Nothing -> Leaf
  Just (l, m, r) -> Node (unfold f l) m (unfold f r)

treeBuild ::
  -- | how many layers for the Tree?
  Integer ->
  BinaryTree Integer
treeBuild n = unfold (\a -> buildT n a) 0

buildT ::
  Integer ->
  Integer ->
  Maybe (Integer, Integer, Integer)
buildT n i
  | n <= 0 || i == n = Nothing
  | otherwise = Just (i + 1, i, i + 1)

main :: IO ()
main = mapM_ go [0..4]
  where go i = putStrLn ("treeBuild " ++ show i ++ ": " ++ show (treeBuild i))
