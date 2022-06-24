module TRS
     ( processTRS
     , processTRSsimple
     , Rule(..)
     , Func(..)
     ) where

import Data.Text as T hiding (tail, map, head, null, length)
import Prelude hiding (lookup)

import Data.Map.Strict hiding (null, map)
import Data.Set as Set hiding(map)
import qualified Data.Map.Strict as Map
import Text.TeXMath.Readers.TeX
import Text.TeXMath.Types as TXT
import Data.Either
import Data.String
import Data.Strings
import Data.ByteString.Lazy.Char8 as BS hiding (tail, map, head, null, length)
import Data.Aeson as JSON
import GHC.Generics

data Output =
  Output { errors :: String
         , funcs  :: Map String Int
  }
instance ToJSON Output where
  toJSON (Output errors funcs) =
    object [ "errors" .= errors
           , "funcs"  .= funcs
    ]

data Func = C String
          | F String [Func]
          | P String
          | ErF String
  deriving Eq
instance Show Func where
  show (ErF er) = er
  show (P x) = x
  show (C b) = b
  show (F a f) = a ++ show f

arity :: Func -> Int
arity (C _) = 0
arity (F _ params) = length params

name :: Func -> String
name (C n) = n
name (F n _) = n
name _ = ""

data Rule = Rl {lhs::Func, rhs::Func}
          | None
  deriving Eq
instance Show Rule where
  show (Rl f1 f2) = show f1 ++ "=" ++ show f2
  show None = "None"

formOutput :: ([Either String Rule], Map String Int, Set String) -> ByteString
formOutput (trs, ars, _) 
  | Map.null ars = JSON.encode $ Output "TRS need to contain at least one function" ars
  | otherwise = JSON.encode $ Output (getErrors 1 trs) ars
  where
    getErrors :: Int -> [Either String Rule] -> String
    getErrors n [] = ""
    getErrors n (r:rs) =
      case r of
        Left er -> "rule " ++ show n ++ ": " ++ er ++ "\n" ++ getErrors (n + 1) rs
        Right _ -> getErrors (n + 1) rs

processTRSsimple :: String -> ByteString
processTRSsimple str = formOutput $ formRules $ fromRight [ENumber "0"] $ readTeX $ T.pack str

processTRS :: String -> ([Rule], Map String Int, Set String)
processTRS str = 
  let (rs, ars, ps) = formRules $ fromRight [ENumber "0"] $ readTeX $ T.pack str
  in (map rightR rs, ars, ps)

rightR :: Either String Rule -> Rule
rightR (Right i) = i
rightR (Left i) = error i

formRules :: [TXT.Exp] -> ([Either String Rule], Map String Int, Set String)
formRules (t:ts) =
  let ps = params t
  in let (t':ts', trs, ars) = rules ts Map.empty ps
    in
      case t' of
        ESymbol Ord "!" -> (trs, ars, ps)
        _ -> error $ "Incorrect syntax -- " ++ show ts'
  where
    params :: TXT.Exp -> Set String
    params t@(EDelimited "{" "}" ts) =
      let ps = map (fromRight (ENumber "0")) ts ++ [ESymbol Pun ";"]
      in params' ps Set.empty

    params' :: [TXT.Exp] -> Set String -> Set String
    params' (t:ts) ps =
      case t of
        TXT.EIdentifier a ->
          let ps' = Set.insert (T.unpack a) ps
          in
            case head ts of
              ESymbol Pun "," -> params' (tail ts) ps'
              _ -> ps'
        _ -> ps

    rules :: [TXT.Exp] -> Map String Int -> Set String -> ([TXT.Exp], [Either String Rule], Map String Int)
    rules (t:ts) ars ps =
        case t of
          TXT.EIdentifier _ ->
            let (ts', rl, ars') = rule (t:ts) ars ps
            in let (ts'', rls, ars'') = rules ts' ars' ps
              in case rl of
                Left er -> (ts'', Left er : rls, ars'')
                _ -> (ts'', rl : rls, ars'')
          ESymbol Pun ";" -> rules ts ars ps
          ESymbol Ord "!"  -> (t:ts, [], ars)
          a -> 
            let (ts', rls', ars') = rules (toNextRule (t:ts)) ars ps
            in (ts', (Left $ show a ) : rls', ars')

    rule :: [TXT.Exp] -> Map String Int -> Set String -> ([TXT.Exp], Either String Rule, Map String Int)
    rule ts ars ps =
      let (t':ts', lhs, ars') = function ts ars ps
      in
        case lhs of
          ErF er -> (toNextRule ts, Left er, ars')
          _ -> case t' of
            TXT.ESymbol Rel "=" ->
              let (ts'', rhs, ars'') = function ts' ars' ps
              in case head ts'' of
                ESymbol Pun ";" -> (tail ts'', Right (Rl lhs rhs), ars'')
                _ -> (toNextRule ts, Left (show $ head ts''), ars'')
            _ -> (toNextRule ts', Left (show t'), ars')

    function :: [TXT.Exp] -> Map String Int -> Set String -> ([TXT.Exp], Func, Map String Int)
    function (t1:ts) ars ps =
      case t1 of
        TXT.EIdentifier a ->
          if Set.notMember (T.unpack a) ps
          then case head ts of
            EDelimited "(" ")" ts' -> let (tsbr, funcs, ars') = function' (map (fromRight (ENumber "0")) ts' ++ [ESymbol Pun ";"]) ars ps []
              in
                case head tsbr of
                  ESymbol Pun ";" ->
                    if strNull $ hasError funcs
                    then (tail ts, checkFunc (F (T.unpack a) funcs) ars', Map.insert (T.unpack a) (length funcs) ars')
                    else (ts, ErF (hasError funcs), ars')
                  _ -> (ts, ErF $ show (head tsbr), ars')
            _ -> (ts, checkFunc (C (T.unpack a)) ars, Map.insert (T.unpack a) 0 ars)
          else (ts, P (T.unpack a), ars)
        _ -> (t1:ts, ErF $ show t1, ars)

    function' :: [TXT.Exp] -> Map String Int -> Set String -> [Func] -> ([TXT.Exp], [Func], Map String Int)
    function' ts ars ps fs =
      let (t':ts', func, ars') = function ts ars ps
      in
        case t' of
          ESymbol Pun "," -> function' ts' ars' ps $ fs ++ [func]
          _ -> (t':ts', fs ++ [func], ars')


    checkFunc :: Func -> Map String Int -> Func
    checkFunc f m =
      case Map.lookup (name f) m of
        Nothing -> f
        Just ar ->
          if ar == arity f
          then f
          else ErF (name f ++ " has different arity in other rules")
          
    hasError :: [Func] -> String
    hasError [] = ""
    hasError (e@(ErF er):_) = er
    hasError (f:fs) = hasError fs
    
    
    toNextRule :: [TXT.Exp] -> [TXT.Exp]
    toNextRule ts
      | head ts == ESymbol Pun ";" = tail ts
      | otherwise = toNextRule $ tail ts

