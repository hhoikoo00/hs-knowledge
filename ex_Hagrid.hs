import Formula
import Logic

rain, hagrid, dumbledore :: Formula
rain = Var "rain"
hagrid = Var "hagrid"
dumbledore = Var "dumbledore"

knowledge :: Formula
knowledge
  = ands [
    (Not rain) `Impl` hagrid,
    hagrid `Or` dumbledore,
    Not hagrid `Or` dumbledore,
    dumbledore
  ]

query :: Formula
query
  = rain

main :: IO ()
main
  = do
      putStrLn ("Knowledge is " ++ show knowledge)
      putStrLn ("Query is " ++ show query)
      let result = modelCheck knowledge query
      putStrLn ("Does the model entail? (using modelCheck): " ++ show result)
      let result = checkWithUnSAT knowledge query
      putStrLn ("Model? (using allSat): " ++ show result)
