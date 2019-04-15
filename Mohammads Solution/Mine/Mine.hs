module Mine (     parseJString,
                  parseJNumber,
                  parseJBool,
                  parseJNull,
                  JValue(..),
		              parseJValue

                  ) where
  
import Text.Parsec
import Text.Parsec.String
import Data.Char(toLower)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

parseJString, parseJNumber, parseJBool, parseJNull :: Parser JValue
parseJString  = do
    str <- between (char '"') (char '"') (many (noneOf "\"")) 
    return . JString $ str
    

parseJNumber = do
    num <- many1 (oneOf "0123456789")
    return $ JNumber (read  num :: Double)

parseJBool = do
    val <- string "true" <|> string "false"
    case val of
        "true"  -> return (JBool True)
        "false" -> return (JBool False)

parseJNull = string "null" >> return JNull

parseJValue :: Parser JValue
parseJValue  =   parseJString  
            <|> parseJNumber  
            <|> parseJBool  
            <|> parseJNull 





            
 