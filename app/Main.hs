module Main where

import System.Environment(getArgs)
import System.Exit
import System.IO
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
        if fileExists then do
          contents <- readFile f
          case lexAndParse contents of
              Left s -> do
                  hPutStrLn stderr $ "Error: parsing failed: " ++ s
                  exitFailure
              Right tree -> interpret tree else do
          hPutStrLn stderr $ "Error: `" ++ f ++ "`: not a file, exiting."
          exitFailure
      _ -> do
        hPutStrLn stderr "Error: need a file name as argument"
        exitFailure