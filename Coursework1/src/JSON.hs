-- Pragma makes type synonyms like 'String' legal instances of JSON
module JSON (
            toJSON,
            toXML,
            getString,
            getInt,
            getPairs,
            getArray,
            JValue(..)
            ) where

    import Data.Char(isDigit, digitToInt, isAlpha)
    import Data.List(intersperse)

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

    -- Convert a string containing a JValue expresion into a JValue.
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
            let pairList = breakByComma $ removeSurrounding '{' '}' xs
                tupleList = map getNamePair pairList
                res = foldr pairing [] tupleList
            in  JObject (res)
        | x == '['              = JArray (map toJSON $ breakByComma $ 
                                    removeSurrounding '[' ']' xs)
        | x == ' '              = toJSON(dropWhile (==' ') xs)
        | otherwise             = JNull
            where pairing (name, value) ys = (name, toJSON value) : ys

    -- Turn a JValue into a string of XML, including newlines and tab spaces.
    -- Used mainly to print to the console using 'putStr'
    toXML  :: JValue -> String
    toXML j =  toXML' j 0
        where
            toXML' (JString s)  _     = s
            toXML' (JNumber n)  _     = show n
            toXML' (JBool   b)  _     = show b
            toXML' (JNull    )  _     = ""
            toXML' (JObject xs) ind     = "\n" ++ (objectToXML (JObject xs) ind)
            toXML' (JArray xs)  _     = (concat (intersperse " " $ map toXML xs))
            room i = replicate i ' '
            objectToXML :: JValue -> Int -> String
            objectToXML (JObject xs) indent = loop xs
                where
                    loop []                    = []
                    loop ((name , value) : xs) = 
                        {-"\n" ++ -}
                        (room indent) ++ "<" ++ name ++ ">" ++ 
                        (toXML' value (indent + 4))  ++ 
                        (isObj value) ++ "</" ++ name ++ ">\n"
                        ++ (objectToXML (JObject xs) (indent))
                            where
                                isObj (JObject _) = (room indent)
                                isObj _           = ""

    getInt :: JValue -> Int
    getInt     (JNumber n)  = truncate n
    getInt  _               = 0

    getArray :: JValue -> [JValue]
    getArray   (JArray xs)  = xs
    getArray _              = []

    getPairs :: JValue -> [(String, JValue)]
    getPairs (JObject xs) = xs
    getPairs _            = []

    getString :: JValue -> String
    getString   (JString s) = s
    getString    _          = ""

    {- HELPER FUNCTIONS -}

    -- Take a string and change it to numbers.
    -- "420" = 420
    -- Should've just used read
    asInt :: [Char] -> Int
    asInt []         = 0
    asInt ('-' : []) = error "Minus but no numbers."
    asInt (x : xs)
        | x == '-'       = (-1) * (foldl step 0 xs)
        | otherwise      = foldl step 0 (x : xs)
        where step x y = (x * 10) + digitToInt y

    -- Take the string after a decimal point and turn it into a double.
    asDecimal :: [Char] -> Double
    asDecimal []        = 0
    asDecimal xs = (fromIntegral(asInt xs)) / (10 ^ (length xs))

    -- Parses a number string like "246.2" and returns 246.2
    asNumber :: [Char] -> Double
    asNumber []         = 0
    asNumber xs
        | (head xs) == '-'   = (fromIntegral $ asInt x) + ((asDecimal (tail y)) * (-1))
        | null y             = fromIntegral(asInt x)
        | otherwise          = (fromIntegral(asInt x)) + (asDecimal (tail y))
        where 
            (x, y) = break (=='.') xs

    -- Get the name pair from a JSON string
    -- e.g. "\"name\"" : \"John\""   ==   ("\"name\"", "\"John\"")
    getNamePair :: String -> (String, String)
    getNamePair s = (cleanString match, rest)
        where (match, rest) = breakBy ':' s

    -- Remove whitespace and escaped strings.
    cleanString :: String -> String
    cleanString s = filter check s
        where check :: Char -> Bool
              check c = (c /= ' ' && c /= '\"')

    -- Remove all whitespace.
    removeSpaces :: String -> String
    removeSpaces = filter (/= ' ')

    -- Remove trailing whitespace.
    removeTrailing :: String -> String
    removeTrailing [] = ""
    removeTrailing s = march 0 s
        where march _ [] = ""
              march acc (x:xs)
                | x == '\"'  = if acc == 0 then x : (march 1 xs) 
                                           else x : (march 0 xs)
                | x == ' '   = if acc == 0 then march acc xs 
                                           else x : (march acc xs)
                | otherwise = x : (march acc xs)

    -- Remove outermost surrounding characters (e.g. {} or [])
    removeSurrounding :: Char -> Char -> String -> String
    removeSurrounding left right s = parse 0 s
        where parse _ []          = []
              parse acc (x:xs)
                | x == left  = 
                    if      acc == 0
                    then    parse (acc + 1) xs
                    else    x : (parse (acc + 1) xs)
                | x == right = 
                    if      acc == 1
                    then    []   
                    else    x : (parse (acc - 1) xs)
                | otherwise =  x : (parse acc xs)

    -- General delimiting function. 
    -- Breaks a list into two, taking into consideration
    -- that the matching character might be inside a string, 
    -- nested object or array.
    breakBy :: Char -> String -> (String, String)
    breakBy c s = march 0 0 [] s
        where
            march _ _ ys [] = (ys, "")
            march acc acc2 ys (x : xs)
                | x == '\"'   = 
                    if   acc == 0
                    then march 1 acc2 (ys ++ [x]) xs
                    else march 0 acc2 (ys ++ [x]) xs
                | x == '['   = march acc (acc2 + 1) (ys ++ [x]) xs
                | x == ']'   = march acc (acc2 - 1) (ys ++ [x]) xs
                | x == '{'   = march acc (acc2 + 1) (ys ++ [x]) xs
                | x == '}'   = march acc (acc2 - 1) (ys ++ [x]) xs
                | x == c     =
                    if   (acc == 0) && (acc2 == 0)
                    then (ys, xs)
                    else march acc acc2 (ys ++ [x]) xs
                | otherwise  = march acc acc2 (ys ++ [x]) xs

    -- Breaks up a string containing commas into a list of lists, 
    -- delimited wherever a comma was found.
    breakByComma :: String -> [String]
    breakByComma s = check s
        where
            check []  = []
            check xs = match : (check rest)
                where
                    (match, rest) = breakBy ',' xs
