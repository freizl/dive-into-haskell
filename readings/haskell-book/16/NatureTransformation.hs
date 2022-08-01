{-# LANGUAGE RankNTypes #-}

module NatureTransformation where

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]


-- This `a+1` will not work, not allowed
-- because compile know nothing about `a`
--
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a + 1]

type Nat2 f g a = f a -> g a

-- This works as it adds more context for `a`.
degenerateMtl :: Num a => Nat2 Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]
