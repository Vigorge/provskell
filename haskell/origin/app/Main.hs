module Main where

import Prelude hiding ((^))
import TRS
import Omega
import Functions as F
import Prove as P
import System.Environment
import Data.ByteString.Lazy.Char8 as BS hiding (null, head)
import Data.Set as Set hiding(null)
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
          then BS.putStrLn $ P.formOutput $ P.prove rules funcs $ Set.elems params
          else BS.putStrLn $ F.formOutput $ F.incorrect funcs
