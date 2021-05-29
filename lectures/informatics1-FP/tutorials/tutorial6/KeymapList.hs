-- INF 1 Functional Programming
-- 
-- Indexed data represented as a list


module KeymapList ( Keymap,
                    size,
                    get, set, del,
                    select,
                    toList, fromList
                  )

where

newtype Eq k => Keymap k a = K [(k,a)]


size :: Eq k => Keymap k a -> Int
size (K xs) = length xs

get :: Eq k => k -> Keymap k a -> Maybe a
get key (K xs) = lookup key xs

set :: Eq k => k -> a -> Keymap k a -> Keymap k a
set key value (K xs) = K (ins  xs)
    where
      ins [] = [(key,value)]
      ins ((k,v):xs) | k == key  = (k,value) : xs
                     | otherwise = (k,v) : ins xs

del :: Eq k => k -> Keymap k a -> Keymap k a
del key (K xs) = K (filter ((/=key).fst) xs)


select :: Eq k => (a -> Bool) -> Keymap k a -> Keymap k a
select f (K xs) = K (filter (f.snd) xs)

toList :: Eq k => Keymap k a -> [(k,a)]
toList (K xs) = xs

fromList :: Eq k => [(k,a)] -> Keymap k a
fromList xs = K xs

