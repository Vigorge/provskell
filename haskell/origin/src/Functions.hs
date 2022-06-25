module Functions
     ( Expr(..)
     , Function(..)
     , processF
     , processInc
     , incorrect
     ) where

import Data.Text as T hiding (tail, map, head, null, length)
import Data.Text.Read
import Prelude hiding (lookup)

import Data.Map.Strict hiding (null, map)
import qualified Data.Map.Strict as Map
import Text.TeXMath.Readers.TeX
import Text.TeXMath.Types as TXT
import Data.Either
import Data.String
import TRS
import Omega
import Data.Maybe
import Data.Data
import Data.ByteString.Lazy.Char8 as BS hiding (tail, map, head, null, elem, length)
import Data.Aeson as JSON
import GHC.Generics

data Expr = Param String
          | Val Omega
          | Sum Expr Expr
          | Mul Expr Expr
          | Exp Expr Expr
          | ErE String
instance Show Expr where
  show = treeShow "\n"
    where
      treeShow :: String -> Expr -> String
      treeShow pref (Param a) = pref ++ a
      treeShow pref (Val v) = pref ++ show v
      treeShow pref (ErE v) = pref ++ show v
      treeShow pref (Mul v1 v2) = pref ++ "╷――――" ++ treeShow (pref ++ "│ ") v1 ++ pref ++ "├─ *" ++ treeShow (pref ++ "│ ") v2 ++ pref ++ "╵――――"
      treeShow pref (Sum v1 v2) = pref ++ "╷――――" ++ treeShow (pref ++ "│ ") v1 ++ pref ++ "├─ +" ++ treeShow (pref ++ "│ ") v2 ++ pref ++ "╵――――"
      treeShow pref (Exp v1 v2) = pref ++ "╷――――" ++ treeShow (pref ++ "│ ") v1 ++ pref ++ "├─ ^" ++ treeShow (pref ++ "│ ") v2 ++ pref ++ "╵――――"

data Function = Fn {sign::[String], expr::Expr}
instance Show Function where
  show fn@(Fn sign expr)= show sign ++ "\n" ++ show expr

data Output =
  Output { errors :: String
         , dummy :: String
  }
instance ToJSON Output where
  toJSON (Output errors dummy) =
    object [ "errorsF" .= errors
           , "errorsR" .= dummy
    ]
 
processInc :: Map String Function -> ByteString
processInc funcs = formOutput $ incorrect funcs
   
formOutput :: [(String, Function)] -> ByteString
formOutput fs = JSON.encode $ Output (getErrors fs) ""
  where
    getErrors :: [(String, Function)] -> String
    getErrors [] = ""
    getErrors (r@(n, Fn {sign=_ ,expr=ErE er}):rs) = show n ++ ": " ++ er ++ "\n" ++ getErrors rs
    getErrors (r@(n, Fn {sign=["err"] ,expr=_}):rs) = show n ++ ": error in signature\n" ++ getErrors rs

incorrect :: Map String Function -> [(String, Function)]
incorrect fs = [x | x@(_, Fn {sign=_ ,expr=ErE {}}) <- Map.assocs fs] ++ [x | x@(_, Fn {sign=["err"] ,expr=_}) <- Map.assocs fs]

processF :: String -> Map String Int -> Map String Function
processF str ars = parse ars $ fromRight [ENumber "0"] $ readTeX $ T.pack str

parse :: Map String Int -> [TXT.Exp] -> Map String Function
parse ars ts =
  let (t':ts', fs) = functions ts ars Map.empty
  in
    case t' of
      ESymbol Ord "!" -> fs
      _ -> error $ "incorrect syntax: " ++ show t'
  where
    functions :: [TXT.Exp] -> Map String Int -> Map String Function -> ([TXT.Exp], Map String Function)
    functions (t:ts) ars fs =
        case t of
          TXT.EIdentifier a ->
            let (ts', fs') = function (t:ts) ars fs
            in functions ts' ars fs'
          _   -> (t:ts, fs)

    function :: [TXT.Exp] -> Map String Int -> Map String Function -> ([TXT.Exp], Map String Function)
    function (t1@(TXT.EIdentifier f):ts) ars fs =
      let (t2:ts', fname) = functionName (t1:ts)
      in case t2 of
        EDelimited "(" ")" ts'' ->
          let s = map (fromRight (ENumber "0")) ts''
          in let sign = if null s then [] else signature (s ++ [ESymbol Pun ";"])
            in
              case head ts' of
                TXT.ESymbol Rel "=" ->
                  if sign /= ["err"] && checkSign (length sign) fname ars
                  then let (t':ts''', ex) = expression (tail ts') sign
                    in case t' of
                      ESymbol Pun ";" -> (ts''', Map.insert fname (Fn {sign=sign, expr=ex}) fs)
                      _ -> (toNextRule (t':ts'''), Map.insert fname (Fn {sign=sign, expr=ErE $ show t'}) fs)
                  else (toNextRule ts, Map.insert fname (Fn {sign=["err"], expr=Param ""}) fs)
                _ -> (toNextRule ts, Map.insert fname (Fn {sign=sign, expr=ErE $ show $ head ts'}) fs)
        _ -> (toNextRule ts, Map.insert fname (Fn {sign=[], expr=ErE $ show t2}) fs)

    function ts _ fs = (toNextRule ts, fs)

    signature :: [TXT.Exp] -> [String]
    signature (t:ts) =
      case t of
        TXT.EIdentifier a ->
          case head ts of
            ESymbol Pun "," ->
              let tl = signature (tail ts)
              in
                if tl == ["err"]
                then tl
                else (T.unpack a):tl
            ESymbol Pun ";" -> [T.unpack a]
            _ -> ["err"]
        _ -> ["err"]

    expression :: [TXT.Exp] -> [String] -> ([TXT.Exp], Expr)
    expression ts ar =
      let (t':ts', lterm) = term ts ar
      in if isSum t'
        then expression' (t':ts') ar lterm
        else (t':ts', lterm)

    expression' :: [TXT.Exp] -> [String] -> Expr -> ([TXT.Exp], Expr)
    expression' (t:ts) ar lterm =
      if isSum t
      then let (ts', rterm) = term ts ar
        in expression' ts' ar (checkErr (Sum lterm rterm) lterm rterm)
      else (t:ts, lterm)

    term :: [TXT.Exp] -> [String]-> ([TXT.Exp], Expr)
    term [] _ = ([], ErE "error in (...)")
    term ts ar =
      let (t':ts', lfactor) = factor ts ar
      in if isMul t'
        then term' (t':ts') ar lfactor
        else (t':ts', lfactor)

    term' :: [TXT.Exp] -> [String] -> Expr -> ([TXT.Exp], Expr)
    term' (t:ts) ar lfactor =
      if isMul t
      then let (ts', rfactor) = factor ts ar
        in term' ts' ar (checkErr (Mul lfactor rfactor) lfactor rfactor)
      else (t:ts, lfactor)
        
    factor :: [TXT.Exp] -> [String]-> ([TXT.Exp], Expr)
    factor [] _ = ([], ErE "error in (...)")
    factor (t:ts) ar =
      case t of
        TXT.ENumber num ->
          (ts, Val $ fromInteger $ right $ decimal num)
        TXT.EIdentifier "\969" ->
          (ts, Val $ w 1)
        TXT.EIdentifier a ->
          if T.unpack a `elem` ar
          then (ts, Param (T.unpack a))
          else (t:ts, ErE "undefined parametr")
        TXT.EDelimited "(" ")" ts' ->
          let (_, ex) = expression (map (fromRight (ENumber "0")) ts' ++ [ESymbol Pun ";"]) ar
          in (ts, ex)
        TXT.ESuper ex up ->
          case up of
            EGrouped gup ->
              let ((_, ex'), (_, up')) = (expression [ex, ESymbol Pun ";"] ar, expression (gup ++ [ESymbol Pun ";"]) ar)
              in (ts, checkErr (Exp ex' up') ex' up')
            _ ->
              let ((_, ex'), (_, up')) = (expression [ex, ESymbol Pun ";"] ar, expression [up, ESymbol Pun ";"] ar)
              in (ts, checkErr (Exp ex' up') ex' up')
        _ -> (t:ts, ErE $ show t)

    functionName :: [TXT.Exp] -> ([TXT.Exp], String)
    functionName (t@(TXT.EIdentifier a):ts) =
      let (ts', nm) = functionName ts
      in (ts', T.unpack a ++ nm)
    functionName ts = (ts, "")

    checkErr :: Expr -> Expr -> Expr -> Expr
    checkErr orig lhs rhs
      | isErr lhs = lhs
      | isErr rhs = rhs
      | otherwise = orig

    isErr :: Expr -> Bool
    isErr (ErE _) = True
    isErr _ = False

    isId :: TXT.Exp -> Bool
    isId (TXT.EIdentifier _) = True
    isId _ = False

    isEq :: TXT.Exp -> Bool
    isEq (TXT.ESymbol Rel "=") = True
    isEq _ = False

    isMul :: TXT.Exp -> Bool
    isMul (ESymbol Bin "\8901") = True
    isMul _ = False

    isSum :: TXT.Exp -> Bool
    isSum (ESymbol Bin "+") = True
    isSum _ = False

    getId :: TXT.Exp -> String
    getId (TXT.EIdentifier i) = T.unpack i

    getArity :: String -> Map String Int -> Int
    getArity i ars = fromMaybe 0 $ Map.lookup i ars

    toNextRule :: [TXT.Exp] -> [TXT.Exp]
    toNextRule ts
      | head ts == ESymbol Pun ";" = tail ts
      | otherwise = toNextRule $ tail ts

    right :: Integral a0 => Either String (a0, Text) -> a0
    right (Right (i, _)) = i

    checkSign :: Int -> String -> Map String Int -> Bool
    checkSign arity f m =
      case Map.lookup f m of
        Nothing -> False
        Just ar -> ar == arity