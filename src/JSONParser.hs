module JSONParser (
                  parseJSONtoXML
                  ,parseJSON
                  ,numberOfElements
                  ,avgPass
                  ,getMostPass
                  ,getLeastPass
                  ,getByState
                  ,getByYear
                  ,getByCode
                  ) where

    import Parser.JSON
    import Data.Char(toLower)
    import Data.Maybe (fromMaybe)
    import Control.Applicative ((<*>))

    type Year       = Int
    type Count      = Int
    type State      = String
    type StateCode  = String
    type Row        = (Year, Count, State, StateCode)

    -- specific data type to hold the input.
    data PassportData =    PData [Row]
        deriving(Show, Eq)

    data Field = Year | Count | State | StateCode deriving (Eq,Show)
    -- Takes a filepath, reads the JSON in its file, and outputs 
    -- it in GHCi using putStrLn.
    parseJSONtoXML :: FilePath -> IO ()
    parseJSONtoXML path  =
        do  input <- readFile path
            jsonInput <- parseJSON input
            putStrLn $ toXML jsonInput

    -- Takes a filepath and returns the contents within an IO JValue.
    parseJSON :: FilePath -> IO JValue
    parseJSON s =
        do let noTabOrReturn c = c /= '\t' && c /= '\r'
           jsonString <- readFile s
           return (toJSON $ unwords $ map (filter noTabOrReturn) $ lines jsonString)

    parseJSONPData = parseJSON "statePassportIssuanceByFiscalYear.json"

    {- 
        For the data set with the filename "statePassportIssuanceByFiscalYear.json"
        Taken from http://www.data.gov/
        Contains data about the number of passports issued in each North American state
        from 2007 to 2012
    -}

    -- These functions all use PassportData, the datatype we've defined to hold
    -- the information from the JSON dataset.

    -- Get the number of elements in this set.
    numberOfElements :: IO Int
    numberOfElements = (length . getArray) <$> parseJSONPData

    -- Get all the rows in this dataset.
    getAll :: IO PassportData
    getAll = (PData . map jObjectToPData . getArray) <$> parseJSONPData

    foldPassports :: (Maybe JValue -> Maybe JValue -> Bool)
                     -> IO PassportData
    foldPassports comparisonFunc = 
        do j <- parseJSONPData
           let foldFunc (JArray (y:ys)) = foldl check y ys
               check :: JValue -> JValue -> JValue
               check alpha beta = 
                if comparisonFunc 
                    (lookup "Count" $ getPairs alpha) 
                    (lookup "Count" $ getPairs beta) then alpha else beta
           return $ PData [jObjectToPData (foldFunc j)]

    -- Get record with the highest 'count' field.
    getMostPass :: IO PassportData
    getMostPass =  foldPassports (>)

    -- Get the tuple with the least passports issued in that year.
    getLeastPass :: IO PassportData
    getLeastPass = foldPassports (<)

    -- Generic function for retrieving records containing Strings
    getBy :: Field -> String ->  IO PassportData
    getBy field input = 
        (PData . map jObjectToPData . (filter ((==) (map toLower input) 
            . maybe " " (map toLower . getString) 
            . lookup (show field) . getPairs)) . getArray) <$> parseJSONPData

    -- Filter results by state.
    getByState :: String -> IO PassportData
    getByState inputState = getBy State inputState

    -- Get all the tuples for a certain year.
    getByYear :: Int -> IO PassportData
    getByYear year = 
        (PData . map jObjectToPData . (filter ((==) year 
             . maybe 0 (getInt) 
             . lookup "Year" . getPairs)) . getArray) <$> parseJSONPData

    -- Get a list of passports by the specified state code.        
    getByCode :: String -> IO PassportData
    getByCode code = getBy StateCode code

    -- Find the average number of passports issued between the years 2007 and 2012.
    avgPass :: IO ()
    avgPass = do
       dataset <- getArray <$> parseJSONPData
       let numberOfRecords = length dataset
           numberOfPassports = sum $ map (maybe 0 getInt
                                    . lookup "Count" 
                                    . getPairs ) dataset
       putStrLn $ "The number of records in this dataset: " ++ show numberOfRecords
       putStrLn $ "The number of passports issued: " ++ show numberOfPassports
       putStrLn $ "The average number of passports issued: " 
            ++ show (numberOfPassports `div` numberOfRecords)

    -- Print passport data in XML format.
    pdataToXML :: PassportData -> String
    pdataToXML (PData []) = ""
    pdataToXML (PData xs) = 
        "<data>" ++ "\n"
        ++ concatMap (showPData' 4) xs
        ++ "</data>"
        where
        room i = replicate i ' '
        showPData' ind (y, c, s, sc) = 
            room ind ++ "<record>" ++ "\n" ++
            room (ind + 4) ++ "<year>" ++ show y ++ "</year>" ++ "\n" ++
            room (ind + 4) ++ "<count>" ++ show c ++ "</count>" ++ "\n" ++
            room (ind + 4) ++ "<state>" ++ s ++ "</state>" ++ "\n" ++
            room (ind + 4) ++ "<stateCode>" ++ sc ++ "</stateCode>" ++ "\n" ++
            room ind ++ "</record>" ++ "\n"

    {- Helper functions -}

    -- Convert a JObject to a row of data.
    jObjectToPData :: JValue -> Row
    jObjectToPData j = (yr j, count j, state j, stateCode j)
        where
        yr         = maybe 0 getInt . lookup "Year" . getPairs
        count      = maybe 0 getInt . lookup "Count" . getPairs 
        state      = maybe "" getString . lookup "State" . getPairs 
        stateCode  = maybe "" getString . lookup "Code" . getPairs
