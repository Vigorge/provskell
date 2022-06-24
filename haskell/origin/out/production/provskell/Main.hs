module Main where

import Lib
import System.Environment   
import Data.List

main :: IO ()
main = do  
    arg args <- getArgs                  -- IO [String]
    progName <- getProgName          -- IO String
    putStrLn "The arguments are:"  
    mapM putStrLn arg  
    putStrLn "The program name is:"  
    putStrLn progName
