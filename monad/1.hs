module Main where


-- 
-- ?? diff (>>=) with (.)
-- (.) could glue functions together, why need (>>=)
--
-- ?? how `[1,2,3] >> [5,6]` being parsed
--    it produces [5,6,5,6,5,6]
--    check the primitive implementation of (>>)

{--

instance Monad [ ] where
    (x:xs) >>= f = f x ++ (xs >>= f)
    []     >>= f = []
    return x     = [x]
    fail s       = []

because of this instance definiation, 
`return 2` to be a type of `(Monad a, Num b) => a b`,
therefore, it is a Monad("Parser"), once it is being parsed, it produce result b 

--}
  
main :: IO ()
main = do x <- glueTogetherM 11
          putStrLn $ show x

------------------------------
          
glueTogether :: Num a => a -> a
glueTogether = plusOne . mulTwo . plusOne

plusOne :: Num a => a -> a
plusOne = (+ 1)

mulTwo :: Num a => a -> a
mulTwo = (* 2)

------------------------------

glueTogetherM2 = (\inp -> inp:[] >>= (\y -> (2 * y):[]) >>= (\z -> (1 + z):[]))

glueTogetherM :: (Monad a, Num b) => b -> a b
glueTogetherM x = (return x) >>= plusOneM >>= mulTwoM >>= plusOneM

plusOneM :: (Monad a, Num b) => b -> a b
plusOneM x = return (1 + x)

mulTwoM :: (Monad a, Num b) => b -> a b
mulTwoM x = return (2 * x)
