module Main where

import Types

term, term', atom :: ReadS Term
term = mapP (foldl1 App) (f (many1 term'))
       where f :: ReadS a -> ReadS a
             f g s = filter (null . snd) (g s)

term11  = mapP (f id) (many1 term')
  where f g [x]    = x
        f g (x:xs) = f (App (g x)) xs
term' = mapP fst (atom &&& (space ||| nil))
atom  = lam <|> var <|> paren term
    
var :: ReadS Term
var = mapP f variable
      where f = Var 

lam :: ReadS Term
lam =
  mapP f (lbd &&& variable &&& (many space >>>> sym '.' <<<< many space) &&& term)
  --mapP f (lbd &&& variable &&& sym '.' &&& term)
  where f (((_,v),_),e) = Lam v e

app :: ReadS Term
app  = mapP f (term &&& space &&& term)
       where f ((f, _), e) = App f e

lbd :: ReadS Char
lbd   = sym '\\' <|> sym 'λ'

sym :: Char -> ReadS Char
sym c = char (==c)

-- | 辅助识别左右括号
--
paren :: ReadS b -> ReadS b
paren p =
  -- mapP f ((sym '(' <<<< many space) &&& p &&& (many space >>>> sym ')'))
  mapP f (sym '(' &&& p &&& sym ')')
  where f ((_, x), _) = x

instance Read Term where
    readsPrec _ = term
