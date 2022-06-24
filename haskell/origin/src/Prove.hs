module Prove where
import Data.Text as T hiding (tail, map, head, null)
import Data.Text.Read
import Prelude hiding (lookup, (^))

import Data.Map.Strict hiding (null, map)
import qualified Data.Map.Strict as Map
import Text.TeXMath.Readers.TeX
import Text.TeXMath.Types as TXT
import Data.Either
import Data.String
import TRS
import Omega
import Data.List as List
import Data.Maybe
import Functions
import Data.ByteString.Lazy.Char8 as BS hiding (tail, map, head, null)
import Data.Aeson as JSON
import GHC.Generics

data Output =
  Output { errors :: String
         , dummy :: String
  }
instance ToJSON Output where
  toJSON (Output errors dummy) =
    object [ "errorsR" .= errors
           , "errorsF" .= dummy
    ]

formOutput :: [(Rule, Map String Integer, Omega, Omega)] -> ByteString
formOutput rs = JSON.encode $ Output (getErrors rs) ""
  where
    getErrors :: [(Rule, Map String Integer, Omega, Omega)] -> String
    getErrors [] = ""
    getErrors (r@(rule, params, lhs, rhs):rs) = show rule ++ "\n  for " ++ formOutputParams (List.sortOn snd $ Map.toAscList params) ++ " got " ++ show lhs ++ "=" ++ show rhs ++ "\n" ++ getErrors rs

formOutputParams :: [(String, Integer)] -> String
formOutputParams [p@(pr, _)] = pr
formOutputParams (p@(pr1, v1):p'@(pr2, v2):ps) =
  pr1 ++ (if v1 < v2 then " < " else " == ") ++ formOutputParams (p':ps)

formVariants :: [[String]] -> [Map String Integer]
formVariants pms = List.nub $ getMapsAll pms

getMapsAll :: [[String]] -> [Map String Integer]
getMapsAll [] = []
getMapsAll (pm:pms) = getMaps pm 10 [] ++ getMapsAll pms

getMaps :: [String] -> Integer -> [(String, Integer)] -> [Map String Integer]
getMaps [] _ mps     = [Map.fromList mps]
getMaps (p:ps) i []  = getMaps ps i [(p, i)]
getMaps (p:ps) i mps = getMaps ps i ((p, i):mps) ++ getMaps ps (i + i) ((p, i + i):mps)

prove :: [Rule] -> Map String Function -> [String] -> [(Rule, Map String Integer, Omega, Omega)]
prove rs fs ps = Prelude.concatMap (checkEvery fs (formVariants $ List.permutations ps)) rs
  where
    checkEvery :: Map String Function -> [Map String Integer] -> Rule -> [(Rule, Map String Integer, Omega, Omega)]
    checkEvery _ [] _ = []
    checkEvery fs (p:ps) r =
      let (ok, lhs, rhs) = correct r fs p
        in if ok
        then checkEvery fs ps r
        else [(r, p, lhs, rhs)]

    correct :: Rule -> Map String Function -> Map String Integer -> (Bool, Omega, Omega)
    correct r fs ps =
      let (lExpr, rExpr) = (compute fs ps (lhs r), compute fs ps (rhs r))
        in (lExpr > rExpr, lExpr, rExpr)

    compute :: Map String Function -> Map String Integer -> Func -> Omega
    compute fs ps (P x)      = unfold $ Tower (fromMaybe 1 $ Map.lookup x ps) 0
    compute fs ps (C name)   = computeExpr [1] (expr $ fromMaybe (Fn [] (Param "")) $ Map.lookup name fs) []
    compute fs ps (F name f) =
      let fn@(Fn sign expr) = fromMaybe (Fn [] (Param "")) $ Map.lookup name fs
      in computeExpr (map (compute fs ps) f) expr sign

    computeExpr :: [Omega] -> Expr -> [String] -> Omega
    computeExpr ps (Param a) sgn = ps !! (fromMaybe 0 $ List.elemIndex a sgn)
    computeExpr _  (Val v)   sgn = v
    computeExpr ps (Sum a b) sgn = (computeExpr ps a sgn) + (computeExpr ps b sgn)
    computeExpr ps (Mul a b) sgn = (computeExpr ps a sgn) * (computeExpr ps b sgn)
    computeExpr ps (Exp a b) sgn = (computeExpr ps a sgn) ^ (computeExpr ps b sgn)