-- |

module BinaryTree where


data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a
  -> BinaryTree a
  -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b)
  -> BinaryTree a
  -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


t1 :: BinaryTree Integer
t1 = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) t1 == mapExpected
  then print "yup OK!"
  else error "test failed!"

{-
  what is preorder, inorder and postorder traversal?
  https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/
-}
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]


t2 :: BinaryTree Integer
t2 = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder t2 == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder t2 == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder t2 == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "Bad news bears"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree = undefined
