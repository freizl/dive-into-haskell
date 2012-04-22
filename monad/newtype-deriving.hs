{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where 

import Control.Monad.Writer

main = do
  ns <- execWriterT $ unRuleM $  multipleTell >> oneTell
  print ns

type IntWriter = [Int]
type AW = WriterT IntWriter IO

-- | A wrapper
--   deriving monad and its monad behavious like its origin type which is `WriterT` here.
--   So, question is why need another wrapper?
newtype RuleM a = RuleM 
    { unRuleM :: AW a
    } deriving (Functor,Monad)

multipleTell :: RuleM ()
multipleTell = RuleM $ do
  tell [1]
  tell [2]
  return ()

oneTell :: RuleM ()
oneTell = RuleM $ do
  tell [3]
  return ()

