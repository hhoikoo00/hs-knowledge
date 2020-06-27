{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module Logic where

import Formula
import DPLL

p, q, r :: Formula
p = Var "P"
q = Var "Q"
r = Var "R"

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

knowledge :: Formula
knowledge
  = ands [
    (p `And` Not q) `Impl` r,
    p,
    Not q
  ]

query :: Formula
query
  = r

main :: IO ()
main
  = do
      putStrLn ("Knowledge is " ++ show knowledge)
      putStrLn ("Query is " ++ show query)
      let result = modelCheck knowledge query
      putStrLn ("Does the model entail? (using modelCheck): " ++ show result)
