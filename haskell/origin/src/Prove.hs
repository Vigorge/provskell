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
import Data.Set as Set hiding(null, map)
import TRS
import Omega
import Data.List as List
import Data.Maybe
import Functions
import Data.ByteString.Lazy.Char8 as BS hiding (tail, map, head, null)
import Data.Aeson as JSON
import GHC.Generics
import Data.Strings as S

data Output =
  Output { errors :: String
         , dummy :: String
  }
instance ToJSON Output where
  toJSON (Output errors dummy) =
    object [ "errorsR" .= errors
           , "errorsF" .= dummy
    ]

processProve :: Integer -> [Rule] -> Map String Function -> Set String -> ByteString
processProve dim rules funcs params = formOutput $ prove dim rules funcs $ Set.elems params

formOutput :: [(Rule, Map String [Integer], [Omega], [Omega])] -> ByteString
formOutput rs = JSON.encode $ Output (getErrors rs) ""
  where
    getErrors :: [(Rule, Map String [Integer], [Omega], [Omega])] -> String
    getErrors [] = ""
    getErrors (r@(rule, params, lhs, rhs):rs) = show rule ++ "\n  for " ++ formOutputParams (List.sortOn snd $ Map.toAscList params) ++ " got " ++ show lhs ++ "=" ++ show rhs ++ "\n" ++ getErrors rs

formOutputParams :: [(String, [Integer])] -> String
formOutputParams [p@(pr, _)] = pr
formOutputParams (p@(pr1, v1):p'@(pr2, v2):ps) =
  pr1 ++ (if v1 < v2 then " < " else " == ") ++ formOutputParams (p':ps)

formVariants :: [String] -> [[String]] -> [Map String [Integer]]
formVariants ps pms = map (getMapByParams ps) (List.nub $ getMapsAll pms)

getMapByParams :: [String] -> Map String Integer -> Map String [Integer]
getMapByParams ps m = Map.fromList $ map (\p -> (p, getMapByParam p m)) ps

getMapByParam :: String -> Map String Integer -> [Integer]
getMapByParam p m = Map.elems $ Map.filterWithKey (\k _ -> S.strStartsWith k p) m

getMapsAll :: [[String]] -> [Map String Integer]
getMapsAll [] = []
getMapsAll (pm:pms) = getMaps pm 10 [] ++ getMapsAll pms

getMaps :: [String] -> Integer -> [(String, Integer)] -> [Map String Integer]
getMaps [] _ mps     = [Map.fromList mps]
getMaps (p:ps) i []  = getMaps ps i [(p, i)]
getMaps (p:ps) i mps = getMaps ps i ((p, i):mps) ++ getMaps ps (i + i) ((p, i + i):mps)

formDimParams :: Integer -> [String] -> [String]
formDimParams dim ps = Prelude.concatMap (\p -> (map (\i -> p ++ (show i)) [1..dim])) ps

prove :: Integer -> [Rule] -> Map String Function -> [String] -> [(Rule, Map String [Integer], [Omega], [Omega])]
prove dim rs fs ps = Prelude.concatMap (checkEvery fs (formVariants ps $ List.permutations (formDimParams dim ps))) rs
  where
    checkEvery :: Map String Function -> [Map String [Integer]] -> Rule -> [(Rule, Map String [Integer], [Omega], [Omega])]
    checkEvery _ [] _ = []
    checkEvery fs (p:ps) r =
      let (ok, lhs, rhs) = correct r fs p
        in if ok
        then checkEvery fs ps r
        else [(r, p, lhs, rhs)]

    correct :: Rule -> Map String Function -> Map String [Integer] -> (Bool, [Omega], [Omega])
    correct r fs ps =
      let (lExpr, rExpr) = (compute fs ps (lhs r), compute fs ps (rhs r))
        in (lExpr > rExpr, lExpr, rExpr)

    compute :: Map String Function -> Map String [Integer] -> Func -> [Omega]
    compute fs ps (P x)      = map unfold (map (\d -> Tower d 0) (fromMaybe [1] $ Map.lookup x ps))
    compute fs ps (C name)   = map (computeExpr [[1]] []) (expr $ fromMaybe (Fn [] [Param ""]) $ Map.lookup name fs)
    compute fs ps (F name f) =
      let fn@(Fn sign exprs) = fromMaybe (Fn [] [Param ""]) $ Map.lookup name fs
      in map (computeExpr (map (compute fs ps) f) sign) exprs

    computeExpr :: [[Omega]] -> [[String]] -> Expr -> Omega
    computeExpr ps sgn (Param a) = (List.concat ps) !! (fromMaybe 0 $ List.elemIndex a $ List.concat sgn)
    computeExpr _  sgn (Val v)   = v
    computeExpr ps sgn (Sum a b) = (computeExpr ps sgn a) + (computeExpr ps sgn b)
    computeExpr ps sgn (Mul a b) = (computeExpr ps sgn a) * (computeExpr ps sgn b)
    computeExpr ps sgn (Exp a b) = (computeExpr ps sgn a) ^ (computeExpr ps sgn b)