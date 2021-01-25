module Main where

import JSONParser(parseJSON)
import XMLParser(parseXML)
import System.Environment(getArgs, getProgName)

main :: IO ()
main = do
        progName <- getProgName
        args <- getArgs
        case args of
            filename:_ -> case dropWhile (/= '.') filename of
                                ".xml"  -> parseXML filename >>= print
                                ".json" -> parseJSON filename >>= print
                                '.':_   -> putStrLn "Unrecognised filetype: expected .xml or .json"
                                _       -> printUsage progName
            _            -> printUsage progName

printUsage :: String -> IO ()
printUsage progName = putStrLn $ "usage: " ++ progName ++ " file"

