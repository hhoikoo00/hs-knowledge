{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module Logic where

import Formula
import DPLL

--------------------------------------------------------------------------
-- Methods of checking the model

-- Brute force model checking approach
modelCheck :: Formula -> Formula -> Bool
modelCheck knowledge query
  = all modelCheck' (sequence allModels)
    where
      symbols   = vars (And knowledge query)
      allModels = [[(symbol, b) | b <- [True, False]] | symbol <- symbols]
      modelCheck' :: [(Id, Bool)] -> Bool
      modelCheck' model
        | evaluate model knowledge  = evaluate model query
        | otherwise                 = True

-- Checking with DPLL algorithm
checkWithUnSAT :: Formula -> Formula -> Bool
checkWithUnSAT knowledge query
  = allSat (knowledge `And` Not query) == []
