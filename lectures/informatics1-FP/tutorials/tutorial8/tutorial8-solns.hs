-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!


import Data.List
import Test.QuickCheck



-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]

states (u, _, _, _, _) = u
alph   (_, a, _, _, _) = a
start  (_, _, s, _, _) = s
final  (_, _, _, f, _) = f
trans  (_, _, _, _, t) = t


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta m state symbol = [ q1 | (q0, x, q1) <- trans m, q0 == state, x == symbol ]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs = acceptsFrom m (start m) xs

acceptsFrom :: (Eq q) => FSM q -> q -> String -> Bool
acceptsFrom m q "" = q `elem` final m
acceptsFrom m q (x:xs) = or [ acceptsFrom m q' xs | q' <- delta m q x ]


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = nub . sort


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta m superq symbol =
    canonical (concat [ delta m q symbol | q <- superq ])

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next m superqs = canonical (
     superqs ++ [ddelta m superq symbol | superq <- superqs, symbol <- alph m])


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable m superqs = if superqs' == superqs then superqs
                      else reachable m superqs'
                          where
                            superqs' = canonical (superqs ++ next m superqs)

-- 8.
containsFinal m superq = or [finalq == q | finalq <- final m, q <- superq]

dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal m superqs = filter (containsFinal m) superqs

--    (alternate solution, using list comprehension and partial application)
-- dfinal m superqs = [superq | superq <- superqs, any (`elem` final m) superq]


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans m superqs = 
    [(sq, symbol, ddelta m sq symbol) | sq <- superqs, symbol <- alph m]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic m = (states, alph m, [start m], dfinal m states, dtrans m states)
  where states = reachable m [[start m]]


-- OPTIONAL MATERIAL ------------------------------------

data Step q = State q 
            | Symbol Char
            deriving (Eq )
                             
type Trace q = [Step q]

instance (Show q) => Show (Step q) where
  show (State q)  = show q
  show (Symbol x) = [x]

-- 11.
      
traces :: Eq q => FSM q -> String -> [Trace q]
traces m  =  trace (start m)
    where
      trace q []     = [ [State q] | q `elem` final m ]
      trace q (x:xs) = [ [State q, Symbol x] ++ t | q1 <- delta m q x, t <- trace q1 xs ]      
      
