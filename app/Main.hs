module Main where

import System.Environment (getArgs, getProgName)

import AST (interp)
import Parser (readExp)

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName

    case args of
        [fileName] -> do
            input <- readFile fileName
            case readExp input of
                Left err -> putStrLn err
                Right x -> putStrLn $ (\(_,_,y) -> y) $ interp x
        _ -> putStrLn $ "Usage: " <> progName <> " [filename]"
