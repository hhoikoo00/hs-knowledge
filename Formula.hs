{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module Formula where

import Data.Maybe
import Data.List

type Id = String

data Formula = Var Id
             | Not Formula
             | And Formula Formula
             | Or  Formula Formula
             | Impl Formula Formula
             | BiCond Formula Formula
             deriving (Eq)

instance Show Formula where
  show (Var v)        = v
  show (Not f)        = '¬' : show f
  show (And f f')     = "(" ++ show f ++ " ∧ " ++ show f' ++ ")"
  show (Or f f')      = "(" ++ show f ++ " ∨ " ++ show f' ++ ")"
  show (Impl f f')    = "(" ++ show f ++ " → " ++ show f' ++ ")"
  show (BiCond f f')  = "(" ++ show f ++ " ↔ " ++ show f' ++ ")"

ands :: [Formula] -> Formula
--Pre: there are at least two formulae in the list argument
ands
  = foldr1 And

ors :: [Formula] -> Formula
--Pre: there are at least two formulae in the list argument
ors
  = foldr1 Or

-- Returns a sorted list of variable names in a formula
vars :: Formula -> [Id]
vars
  = sort . nub . vars'
    where
      vars' (Var x)
        = [x]
      vars' (Not f)
        = vars' f
      vars' (And f f')
        = vars' f ++ vars' f'
      vars' (Or f f')
        = vars' f ++ vars' f'
      vars' (Impl f f')
        = vars' f ++ vars' f'
      vars' (BiCond f f')
        = vars' f ++ vars' f'

evaluate :: [(Id, Bool)] -> Formula -> Bool
evaluate idMap
  = evaluate'
    where
      evaluate' :: Formula -> Bool
      evaluate' (Var x)
        = lookUp x idMap
      evaluate' (Not f)
        = (not . evaluate') f
      evaluate' (And f f')
        = evaluate' f && evaluate' f'
      evaluate' (Or f f')
        = evaluate' f || evaluate' f'
      evaluate' (Impl f f')
        = evaluate' (Not f) || evaluate' f'
      evaluate' (BiCond f f')
        = (evaluate' f && evaluate' f') ||
          (evaluate' (Not f) && evaluate' (Not f'))

--------------------------------------------------------------------------
-- Helper functions

-- Look up an item in a list of (item, value) pairs and return value
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp
  = (fromJust .) . lookup
