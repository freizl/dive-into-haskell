-- | 

module Filtering where

import Data.List

myFilter = filter notArticles . words
notArticles xs = xs `notElem` ["a", "the", "an"]


