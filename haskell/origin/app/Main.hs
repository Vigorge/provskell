module Main where

import Prelude hiding ((^))
import TRS
import Omega
import Calc as C
import Functions as F
import Prove as P
import System.Environment
import Data.ByteString.Lazy.Char8 as BS hiding (null, head, tail)
import Data.List as List

main :: IO ()
main = do
  (modeArg:trsArg:other) <- getArgs
  case modeArg of
    "0" -> BS.putStrLn $ processTRSsimple trsArg
    "1" ->
      let (funcArg, dimArg) = (head other, head (tail other))
      in let (rules, arity, params) = processTRS trsArg
          in let funcs = F.processF funcArg arity (read dimArg :: Integer)
            in if null $ F.incorrect funcs
              then BS.putStrLn $ P.processProve (read dimArg :: Integer) rules funcs params
              else BS.putStrLn $ F.processInc funcs
    "2" -> BS.putStrLn $ C.processCalc trsArg

 {-mapM_ print (P.formVariants ["x", "y"] $ List.permutations (formDimParams 2 ["x", "y"]))-}
