{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module DPLL where

import Formula

--------------------------------------------------------------------------
-- Local Type definitions

type NNF = Formula

type CNF = Formula

type CNFRep = [[Int]]

type IdMap = [(Id, Int)]

--------------------------------------------------------------------------
-- Helper functions

-- Generates a list of variable (Id, Int) pairs for a formula
idMap :: Formula -> IdMap
idMap
  = flip zip [1..] . vars

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

--------------------------------------------------------------------------
-- Obtaining normal forms

toNNF :: Formula -> NNF
-- NNF laws
toNNF (Not (Or f f'))
  = And (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (And f f'))
  = Or (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (Not f))
  = toNNF f
-- Normal structural induction
toNNF (BiCond f f')
  = toNNF (Or (And f f') (And (Not f) (Not f')))
toNNF (Impl f f')
  = toNNF (Or (Not f) (f'))
toNNF (Not f)
  = Not (toNNF f)
toNNF (And f f')
  = And (toNNF f) (toNNF f')
toNNF (Or f f')
  = Or (toNNF f) (toNNF f')
toNNF f
  = f

-- Convert a formula into Conjunctive Normal Form
toCNF :: Formula -> CNF
toCNF
  = toCNF' . toNNF
    where
      -- Convert a NNF formula into CNF formula
      toCNF' :: NNF -> CNF
      toCNF' (Or f f')
        = distribute f f'
      toCNF' (And f f')
        = And (toCNF' f) (toCNF' f')
      toCNF' (Not f)
        = Not (toCNF' f)
      toCNF' f
        = f

-- Flatten a CNF formula
flatten :: CNF -> CNFRep
--Pre: given formula is in CNF
flatten f
  = flatten' f
    where
      idF = idMap f
      flatten' :: CNF -> CNFRep
      flatten' (Var x)
        = [[lookUp x idF]]
      flatten' (Not (Var x))
        = [[-(lookUp x idF)]]
      flatten' (And f f')
        = flatten' f ++ flatten' f'
      flatten' (Or f f')
        = (f1 ++ f2) : fs ++ fs'
          where
            (f1 : fs)   = flatten' f
            (f2 : fs')  = flatten' f'
      flatten' _
        = error "flatten: formula not in CNF"

--------------------------------------------------------------------------
-- The DPLL Algorithm

-- Propagate all possible unit clauses
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits f
  | singleton == [] = (f, [])
  | otherwise       = (f', (u : us))
    where
      singleton = (concat . (filter isSingleton)) f
      (u : _)   = singleton
      (f', us)  = (propUnits . propUnits') f
      propUnits' :: CNFRep -> CNFRep
      propUnits'
        = (map (filter (/=(-u)))) . (filter (not . (u `elem`)))
      isSingleton :: [a] -> Bool
      isSingleton [_]
        = True
      isSingleton _
        = False

-- Implementation of The Davis-Putnam Algorithm for SAT problem
dp :: CNFRep -> [[Int]]
dp f
  | f' == []      = [us]
  | [] `elem` f'  = []
  | otherwise     = map (us++) (dp ([u] : f') ++ dp ([-u] : f'))
    where
      (f', us)      = propUnits f
      ((u : _) : _) = f'

-- Return a list of all satisfiable situations in a formula
allSat :: Formula -> [[(Id, Bool)]]
allSat f
  = (map (map convertToPair) . listAll . toDP) f
    where
      toDP    = dp . flatten . toCNF
      listAll = map reverse . concatMap (getAll fVars [])
      fVars   = [1..length (vars f)]
      fMap    = (map (\(a, b) -> (b, a)) . idMap) f
      getAll :: [Int] -> [Int] -> [Int] -> [[Int]]
      getAll [] acc _
        = [acc]
      getAll (x : xs) acc s
        | x `elem` s  = getAll xs (x : acc) s
        | -x `elem` s = getAll xs (-x : acc) s
        | otherwise   = getAll xs (x : acc) s ++ getAll xs (-x : acc) s
      convertToPair :: Int -> (Id, Bool)
      convertToPair i
        = (lookUp (abs i) fMap, i > 0)
