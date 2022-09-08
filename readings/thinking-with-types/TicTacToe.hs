{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module TicTacToe where

data XO = X | O
  deriving (Eq, Ord, Show, Read)

data Board1 a = Board1
  { btl,
    bt,
    btr,
    bml,
    bm,
    bmr,
    bbl,
    bb,
    bbr ::
      a
  }

data Triple a = Triple
  { t1 :: a,
    t2 :: a,
    t3 :: a
  }

newtype Board2 a = Board2
  { getBoard2 :: Triple (Triple a)
  }

data Three = First | Second | Third
  deriving (Eq, Ord, Show, Read)

newtype Board3 a = Board3
  { getBoard3 :: Three -> Three -> a
  }

empty1 :: Board1 (Maybe a)
empty1 =
  Board1
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

empty2 :: Board2 (Maybe a)
empty2 =
  Board2 $
    Triple
      (Triple Nothing Nothing Nothing)
      (Triple Nothing Nothing Nothing)
      (Triple Nothing Nothing Nothing)

empty3 :: Board3 (Maybe a)
empty3 = Board3 $ const $ const Nothing

getAt1 :: Three -> Three -> Board1 a -> a
getAt1 First First = btl
getAt1 First Second = bt
getAt1 First Third = btr
getAt1 Second First = bml
getAt1 Second Second = bm
getAt1 Second Third = bmr
getAt1 Third First = bbl
getAt1 Third Second = bb
getAt1 Third Third = bbr

setAt1 :: a -> Three -> Three -> Board1 a -> Board1 a
setAt1 a First First b = b {btl = a}
setAt1 a First Second b = b {bt = a}
setAt1 a First Third b = b {btr = a}
setAt1 a Second First b = b {bml = a}
setAt1 a Second Second b = b {bm = a}
setAt1 a Second Third b = b {bmr = a}
setAt1 a Third First b = b {bbl = a}
setAt1 a Third Second b = b {bb = a}
setAt1 a Third Third b = b {bbr = a}

-- | The checkWinner from Algebra.hs doesn't seem correct.
-- @
-- checkWinner :: TicTacToe (Maybe Bool) -> Maybe Bool
-- checkWinner (TicTacToe {..}) = join $ listToMaybe $ do
--   line <- [ [topLeft,   topCenter, topRight]
--           , [midLeft,   midCenter, midRight]
--           , [botLeft,   botCenter, botRight]
--           , [topLeft,   midLeft,   botLeft]
--           , [topCenter, midCenter, botCenter]
--           , [topRight,  midRight,  botRight]
--           , [topLeft,   midCenter, botRight]
--           , [topRight,  midCenter, botLeft]
--           ]
--   let [one, two, three] = line
--   guard $ isJust one
--        && two == one
--        && three == one
--   pure one
-- @
won1 :: Board1 (Maybe Bool) -> Maybe Bool
won1 board = won (\x y -> getAt1 x y board)

getTriple :: Three -> Triple a -> a
getTriple First = t1
getTriple Second = t2
getTriple Third = t3

setTriple :: a -> Three -> Triple a -> Triple a
setTriple a First t = t {t1 = a}
setTriple a Second t = t {t2 = a}
setTriple a Third t = t {t3 = a}

getAt2 :: Three -> Three -> Board2 a -> a
getAt2 y x (Board2 b) = getTriple x $ getTriple y b

setAt2 :: a -> Three -> Three -> Board2 a -> Board2 a
setAt2 a y x (Board2 b) =
  Board2 $
    setTriple (setTriple a x (getTriple y b)) y b

getAt3 :: Three -> Three -> Board3 a -> a
getAt3 x y (Board3 b) = b x y

setAt3 :: a -> Three -> Three -> Board3 a -> Board3 a
setAt3 a x y (Board3 b) =
  Board3 $ \x' y' ->
    if x == x' && y == y'
      then a
      else b x' y'

won3 :: Board3 (Maybe Bool) -> Maybe Bool
won3 (Board3 f) = won f

won :: (Three -> Three -> Maybe Bool) -> Maybe Bool
won getValueAt =
  let checkLines =
        map
          (map (uncurry getValueAt))
          [ [(First, First), (First, Second), (First, Third)],
            [(Second, First), (Second, Second), (Second, Third)],
            [(Third, First), (Third, Second), (Third, Third)],
            [(First, First), (Second, First), (Third, First)],
            [(First, Second), (Second, Second), (Third, Second)],
            [(First, Third), (Second, Third), (Third, Third)],
            [(First, First), (Second, Second), (Third, Third)],
            [(First, Third), (Second, Second), (Third, First)]
          ]
      go xs b = case findWinner xs of
        Just x -> Just x
        Nothing -> b
 in foldr go Nothing checkLines

-- | Is there built method to use??
findWinner :: [Maybe Bool] -> Maybe Bool
findWinner xs =
  if all (== Just True) xs
    then Just True
    else
      if all (== Just False) xs
        then Just False
        else Nothing

main :: IO ()
main = main1 >> main3

main3 :: IO ()
main3 = do
  let ts =
        [ setAt3 (Just True) First Third $
            setAt3 (Just True) First Second $
              setAt3 (Just True) First First empty3,
          setAt3 (Just False) Third Third $
            setAt3 (Just False) Third Second $
              setAt3 (Just False) Third First empty3
        ]
  mapM_ (print . won3) ts

main1 :: IO ()
main1 = do
  let ts =
        [ empty1
            { btl = (Just True),
              bt = (Just True),
              btr = (Just True)
            },
          empty1
            { bbl = (Just False),
              bb = (Just False),
              bbr = (Just False)
            }
        ]
  mapM_ (print . won1) ts
