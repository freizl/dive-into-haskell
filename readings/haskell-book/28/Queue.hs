-- |
module Queue where

-- From Okasaki's Purely
-- Functional Data Structures
data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

def :: Queue a
def = Queue [] []

push :: a -> Queue a -> Queue a
push = undefined

pop :: Queue a -> Maybe (a, Queue a)
pop = undefined
