-- The implementation for the JSON parser.
module Parser.JSON 
(  
            toJSON,
            toXML,
            getString,
            getInt,
            getPairs,
            getArray,
            JValue(..)) where 

    import Data.Char(isDigit, digitToInt, isAlpha, isSpace)
    import Data.List(intersperse)
    import Text.Read(readMaybe)
    import Data.Maybe(fromMaybe)

    data JValue = 
          JString String
        | JNumber Double
        | JBool   Bool
        | JNull
        | JObject [(String, JValue)]
        | JArray  [JValue]
        deriving (Eq, Ord, Show)

    -- Convert a string to a JBool.
    toJBool :: String -> JValue
    toJBool "true"  = JBool True
    toJBool "false" = JBool False
    toJBool _       = JNull

    -- Convert a string containing a JValue expression into a JValue.
    toJSON :: String -> JValue
    toJSON "" = JNull
    toJSON xs@(x : xs')
        | x == '\"'             = JString (filter (/='\"') $ removeTrailing xs)
        | isDigit x             = JNumber (asNumber $ removeSpaces xs)
        | isAlpha x             = 
            if      (take 4 $ removeSpaces xs) == "null"
            then    JNull
            else    toJBool $ removeSpaces xs
        | x == '{'              = 
            let pairList = breakByComma $ removeSurrounding xs
                tupleList = map getNamePair pairList
                res = foldr pairing [] tupleList
            in JObject res
        | x == '['              = JArray (map toJSON $ breakByComma 
                                  $ removeSurrounding xs)
        | x == ' '              = toJSON(dropWhile (==' ') xs)
        | otherwise             = JNull
            where
                pairing (name, value) ys =
                    (name, toJSON value) : ys

    -- Turn a JValue into a string of XML, including newlines and tab spaces.
    -- Used mainly to print to the console using 'putStr'
    toXML  :: JValue -> String
    toXML j =  toXML' j 0
        where
            toXML' (JString s)  _     = s
            toXML' (JNumber n)  _     = show n
            toXML' (JBool   b)  _     = show b
            toXML' JNull        _     = ""
            toXML' (JObject xs) ind     = "\n" ++ objectToXML (JObject xs) ind
            toXML' (JArray xs)  _     = unwords $ map toXML xs
            room i = replicate i ' '
            objectToXML :: JValue -> Int -> String
            objectToXML (JObject xs) indent = loop xs
                where
                    loop []                    = []
                    loop ((name , value) : xs) = 
                        room indent ++ "<" ++ name ++ ">" ++ 
                        toXML' value (indent + 4)  ++ 
                        isObj value ++ "</" ++ name ++ ">\n"
                        ++ objectToXML (JObject xs) indent
                            where
                                isObj (JObject _) = room indent
                                isObj _           = ""

    -- Extract an int from a JNumber or 0 for bad input.
    getInt :: JValue -> Int
    getInt     (JNumber n)  = truncate n
    getInt  _               = 0

    -- Extract a list from a JArray, or [] for bad input.
    getArray :: JValue -> [JValue]
    getArray   (JArray xs)  = xs
    getArray _              = []

    getPairs :: JValue -> [(String, JValue)]
    getPairs (JObject xs) = xs
    getPairs _            = []

    getString :: JValue -> String
    getString   (JString s) = s
    getString _             = ""

    -- Parses a number string like "246.2" and returns 246.2
    asNumber :: String -> Double
    asNumber = fromMaybe 0 . readMaybe

    -- Get the name pair from a JSON string
    -- e.g. "\"name\"" : \"John\"" ("\"name\"", "\"John\"")
    getNamePair :: String -> (String, String)
    getNamePair [] = ("", "")
    getNamePair s = (cleanString match, rest)
        where
            (match, rest) = breakBy ':' s

    -- Remove whitespace and escaped strings.
    cleanString :: String -> String
    cleanString  = removeSpaces . filter (/= '\"')

    -- Remove all whitespace.
    removeSpaces :: String -> String
    removeSpaces = filter (/= ' ')

    -- Remove trailing space from beginning and end of a string.
    removeTrailing :: String -> String
    removeTrailing = reverse . dropWhile isSpace . 
                     reverse . dropWhile isSpace

    -- Remove surrounding chars (like braces, brackets, speech marks..)
    removeSurrounding :: String -> String
    removeSurrounding = tail . init . removeTrailing 

    -- General delimiting function. Breaks a list into two, taking into 
    -- consideration that the matching character might be inside a string, 
    -- nested object or array.
    breakBy :: Char -> String -> (String, String)
    breakBy c s = march False 0 [] s
        where
            march :: Bool -> Int -> String -> String -> (String, String)
            march _ _ ys [] = (ys, "")
            march inString acc ys (x : xs)
                | x == '\"'   = 
                    if      not inString
                    then    march True acc (ys ++ [x]) xs
                    else    march False acc (ys ++ [x]) xs
                | x == '['   = march inString (acc + 1) (ys ++ [x]) xs
                | x == ']'   = march inString (acc - 1) (ys ++ [x]) xs
                | x == '{'   = march inString (acc + 1) (ys ++ [x]) xs
                | x == '}'   = march inString (acc - 1) (ys ++ [x]) xs
                | x == c     =
                    if      (not inString) && (acc == 0)
                    then    (ys, xs)
                    else    march inString acc (ys ++ [x]) xs
                | otherwise  = march inString acc (ys ++ [x]) xs

    -- Breaks up a string containing commas into a list of Strings, delimited 
    -- wherever a comma was found.
    breakByComma :: String -> [String]
    breakByComma s = 
        let check [] = []
            check xs = 
                let (match, rest) = breakBy ',' xs
                in match : (check rest)
            in check s
