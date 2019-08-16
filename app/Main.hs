module Main where

import JSONParser(parseJSON)
import XMLParser(parseXML)

main :: IO ()
main = do
        {-parsedJson <- parseJSON "./app/data/example.json"
        putStrLn $ show parsedJson-}
        parsedXML <- parseXML "./app/data/example.xml"
        putStrLn $ show parsedXML
