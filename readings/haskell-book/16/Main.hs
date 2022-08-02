{-# LANGUAGE FlexibleInstances #-}
-- |

module Main where

a :: [Int]
a = (+1) <$> (read "[1]":: [Int])

-- b :: Maybe String
-- b = (fmap (++ "lol")) <$> (Just ["hi,", "Hello"])

c :: Int -> Int
c = (*2) . (\x -> x - 2)

d :: Int -> String
d = ((return '1' ++) . show) . (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = ( read . ("123" ++) . show ) <$> ioi
    in (*3) <$> changed

t1 = do
  print a
  -- print b
  print (c 1)
  print (d 0)
  e >>= print

main = do
  t1

---
-- * Chapter exercise
---

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

-- apparently data constructor @K a@ doesn't work here
-- give @f@ has to have kind (* -> *)
data K2 a b = K2 a b
-- newtype K a b = K a

instance Functor (Flip K2 a) where
  fmap f (Flip (K2 b a)) = Flip (K2 (f b) a)

data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)


data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt)

data List a = Nil | Cons a (List a)
  deriving (Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)

testList = print $ fmap (+1) (Cons (3::Int) (Cons 4 Nil))

data GoatLord a = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

testGoat = do
  print $ fmap (+2) (OneGoat 3)
  print $ fmap (+2) (MoreGoats (OneGoat 3) (OneGoat 5) NoGoat)

data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Show a => Show (TalkToMe a) where
  show Halt = "TalkToMe: Halt"
  show (Print str a) = "TalkToMe: Print " ++ show (str, a)
  show (Read g) = "TalkToMe: Read " ++ (show $ g "showTestStr")

instance Functor (TalkToMe) where
  fmap _ Halt = Halt
  fmap f (Print str a) = (Print str (f a))
  fmap f (Read g) = Read (f . g)

testTalkToMe = do
  print (fmap (+ 3) Halt)
  print (fmap (+ 4) (Print "print" 5))
  print (fmap (+ 5) (Read (\str -> 6)))
  print (fmap (++ "+changed+") (Read (\str -> str ++ "-default-")))
