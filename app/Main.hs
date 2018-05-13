module Main where

import System.Environment(getArgs)
import System.Exit
import System.Directory(doesFileExist)

import BNFC.ErrM(Err(Ok, Bad))
import BNFC.ParStretch
import BNFC.LexStretch
import BNFC.AbsStretch(Program)

import Lib(interpret)

lexer :: String -> [Token]
lexer = myLexer

parser :: [Token] -> Err Program
parser = pProgram

lexAndParse :: String -> Either String Program
lexAndParse s = case (parser . lexer) s of
    Bad msg -> Left msg
    Ok tree -> Right tree

main :: IO ()
main = do
    args <- getArgs
    case args of
      (f : _) -> do
        fileExists <- doesFileExist f
        case fileExists of
          False -> do
            putStrLn $ "Error: `" ++ f ++ "`: not a file, exiting."
            exitFailure
          True -> do
            contents <- readFile f
            case lexAndParse contents of
                Left s -> do
                    putStrLn $ "Error: parsing failed: " ++ s
                    exitFailure
                Right tree -> interpret tree
      _ -> do
        putStrLn "Error: need a file name as argument"
        exitFailure