module Main where

import Prelude hiding ((^))
import TRS
import Omega
import Calc as C
import Functions as F
import Prove as P
import System.Environment
import Data.ByteString.Lazy.Char8 as BS hiding (null, head)
import Data.List as List

main :: IO ()
main = do
  (modeArg:trsArg:other) <- getArgs
  case modeArg of
    "0" -> BS.putStrLn $ processTRSsimple trsArg
    "1" -> 
      let (rules, arity, params) = processTRS trsArg
        in let funcs = F.processF (head other) arity
          in if null $ F.incorrect funcs
          then BS.putStrLn $ P.processProve rules funcs params
          else BS.putStrLn $ F.processInc funcs
    "2" -> BS.putStrLn $ C.processCalc trsArg

{-
 print $ (w ( w (w 1))) * 2
-}
