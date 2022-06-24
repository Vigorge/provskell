module Calc where
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
         , result :: String
  }
instance ToJSON Output where
  toJSON (Output errors result) =
    object [ "errors" .= errors
           , "result" .= result
    ]
    
processCalc :: String -> ByteString
processCalc str = 
  let expr = expression $ fromRight [ENumber "0"] $ readTeX $ T.pack str
  in 
    case expr of
      ErE er -> formOutput (Left er)
      _ -> formOutput (Right (calculate expr))
    
formOutput :: Either String Omega -> ByteString
formOutput (Left er) = JSON.encode $ Output er ""
formOutput (Right val) = JSON.encode $ Output "" $ latex val

calculate :: Expr -> Omega
calculate (Val v)   = v
calculate (Sum a b) = (calculate a) + (calculate b)
calculate (Mul a b) = (calculate a) * (calculate b)
calculate (Exp a b) = (calculate a) ^ (calculate b)

expression :: [TXT.Exp] -> Expr
expression ts =
  let (t':ts', lterm) = term ts
  in if isSum t'
    then let (t'':_, expr) = expression' (t':ts') lterm
    in if t'' == ESymbol Ord "!"
      then expr
      else ErE $ show t''
    else lterm

expression' :: [TXT.Exp] -> Expr -> ([TXT.Exp], Expr)
expression' (t:ts) lterm =
  if isSum t
  then let (ts', rterm) = term ts
    in expression' ts' (checkErr (Sum lterm rterm) lterm rterm)
  else (t:ts, lterm)

term :: [TXT.Exp] -> ([TXT.Exp], Expr)
term [] = ([], ErE "error in (...)")
term ts =
  let (t':ts', lfactor) = factor ts
  in if isMul t'
    then term' (t':ts') lfactor
    else (t':ts', lfactor)

term' :: [TXT.Exp] -> Expr -> ([TXT.Exp], Expr)
term' (t:ts) lfactor =
  if isMul t
  then let (ts', rfactor) = factor ts
    in term' ts' (checkErr (Mul lfactor rfactor) lfactor rfactor)
  else (t:ts, lfactor)
        
factor :: [TXT.Exp] -> ([TXT.Exp], Expr)
factor [] = ([], ErE "error in (...)")
factor (t:ts) =
  case t of
    TXT.ENumber num ->
      (ts, Val $ fromInteger $ right $ decimal num)
    TXT.EIdentifier "\969" ->
      (ts, Val $ w 1)
    TXT.EDelimited "(" ")" ts' ->
      let ex = expression (map (fromRight (ENumber "0")) ts' ++ [ESymbol Ord "!"])
      in (ts, ex)
    TXT.ESuper ex up ->
      case up of
        EGrouped gup ->
          let (ex', up') = (expression [ex, ESymbol Ord "!"], expression (gup ++ [ESymbol Ord "!"]))
          in (ts, checkErr (Exp ex' up') ex' up')
        _ ->
          let (ex', up') = (expression [ex, ESymbol Ord "!"], expression [up, ESymbol Ord "!"])
          in (ts, checkErr (Exp ex' up') ex' up')
    _ -> (t:ts, ErE $ show t)

checkErr :: Expr -> Expr -> Expr -> Expr
checkErr orig lhs rhs
  | isErr lhs = lhs
  | isErr rhs = rhs
  | otherwise = orig

isErr :: Expr -> Bool
isErr (ErE _) = True
isErr _ = False

isMul :: TXT.Exp -> Bool
isMul (ESymbol Bin "\8901") = True
isMul _ = False

isSum :: TXT.Exp -> Bool
isSum (ESymbol Bin "+") = True
isSum _ = False

right :: Integral a0 => Either String (a0, Text) -> a0
right (Right (i, _)) = i
