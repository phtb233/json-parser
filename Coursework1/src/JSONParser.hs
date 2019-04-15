module JSONParser where
    import JSON
    import Data.Char(toLower)

    -- Takes a filepath, reads the JSON in it's file, and outputs 
    -- it in GHCi using putStrLn.
    parseJSONtoXML :: FilePath -> IO ()
    parseJSONtoXML s  =
        do -- Take the file path and read the contents to jsonString.
            jsonString <- readFile s
            putStrLn $ toXML $ toJSON $ unwords $ lines jsonString

    -- Takes a filepath and returns the contents within an IO JValue.
    parseJSON :: FilePath -> IO JValue
    parseJSON s =
        do
            jsonString <- readFile s
            return (toJSON $ unwords $ lines jsonString)


    {- 
        For the data set with the filename "statePassportIssuanceByFiscalYear.json"
        Taken from http://www.data.gov/
    -}

    -- Find the tuple with the highest passports issued in a year.
    getMostPass :: IO ()
    getMostPass = 
        do
            j <- parseJSON "statePassportIssuanceByFiscalYear.json"
            putStrLn $ toXML $ mostPass j
            where
                mostPass :: JValue -> JValue
                mostPass (JArray (y:ys)) = foldl check y ys
                    where
                        check a b
                            | count1 > count2  = a
                            | otherwise        = b
                            where
                                (_, count1) =  head $ filter isCount $ getPairs a
                                (_, count2) =  head $ filter isCount $ getPairs b
                                isCount (name, value) = name == "Count" 

    -- Get the tuple with the least passports issued in that year.
    getLeastPass :: IO ()
    getLeastPass = 
        do
            j <- parseJSON "statePassportIssuanceByFiscalYear.json"
            putStrLn $ toXML $ leastPass j
                where
                leastPass :: JValue -> JValue
                leastPass (JArray (x : xs)) = march x xs
                    where
                        march a []   = a
                        march a (b : bs)
                            |  count1 < count2 = march a bs
                            | otherwise        = march b bs
                                where
                                    (_, count1) =  head $ filter isCount $ getPairs a
                                    (_, count2) =  head $ filter isCount $ getPairs b
                                    isCount (name, value) = name == "Count"

    -- Filter results by state.
    getByState :: String -> IO ()
    getByState s =
        do
            j <- parseJSON "statePassportIssuanceByFiscalYear.json"
            putStrLn $ toXML $ byState s j
            where
                byState :: String -> JValue -> JValue
                byState s (JArray xs) = JArray (foldr check [] xs)
                    where 
                        check a b 
                            | state' == s'     = a : b
                            | otherwise        = b
                                where
                                   (_, state ) = head $ filter isState $ getPairs a
                                   s' = map toLower s
                                   state' = map toLower $ getString state
                                   isState (name, value) = name == "State" 

    -- Get all the tuples for a certain year.
    getByYear :: Int -> IO ()
    getByYear n = 
        do
            j <- parseJSON "statePassportIssuanceByFiscalYear.json"
            putStrLn $ toXML $ byYear n j
            where
                byYear :: Int -> JValue -> JValue
                byYear n (JArray xs) = JArray (filter isMyYear xs)
                    where
                        isMyYear (JObject ys) = year' == n
                            where
                                (_, year) = head $ filter isYear ys
                                year' = getInt year
                                isYear (name, value) = name == "Year"


    getByCode :: String -> IO ()
    getByCode s = 
        do
            j <- parseJSON "statePassportIssuanceByFiscalYear.json"
            putStrLn $ toXML $ byCode s j
            where
                byCode :: String -> JValue -> JValue
                byCode s (JArray xs) = JArray (filter isMyCode xs)
                    where
                        isMyCode (JObject ys) = code' == map toLower s
                            where
                                (_, code) = head $ filter isCode ys
                                code' = map toLower $ getString code
                                isCode (name, value) = name == "StateCode"


    -- States per year, the number of states surveyed for each year
