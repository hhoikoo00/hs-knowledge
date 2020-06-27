import Formula
import Logic

p, q, r :: Formula
p = Var "P"
q = Var "Q"
r = Var "R"

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
      let result = checkWithUnSAT knowledge query
      putStrLn ("Model? (using allSat): " ++ show result)
