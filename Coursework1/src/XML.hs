-- Read in XML and store it in Haskell data type.

module XML(toXML, XMLValue(..)) where


    import Debug.Trace(trace)
    import Data.Char(isAlpha, isSpace)
    import Data.List((\\), isInfixOf)

    type Name  = String
    type Value = String
    type Attribute = (Name, Value)
    type Contents = [XMLValue]

    -- Accidentaly made the components of XMLElement and XMLClose tuples
    data XMLValue =
       XMLString String
      | XMLElement Name [Attribute] Contents
      | XMLClose   Name [Attribute]             -- <element/> Self closing elements.
      deriving (Show, Eq, Ord, Read)

    data MyData = Foo Int String
                | Bar Bool Double
                deriving Show

    myString = "gfddgfdgf<firstName id=\"123\" ref=\"abc\" den=\"doreme\" fat = \"teedee\">Stephen<initials>SOB</initials> more words</firstName><firstName>Kevin<initials>KMD</initials> guess </firstName>"
    fet = "<initials>SOB</initials>"
    selfClose = "<selfClose/>"

    -- Get the tags and content of the first outermost element (and the rest of the
    -- string) in a tuple (match, rest).
    getNextElement :: String -> (String, String)
    getNextElement s = parse 0 False [] s
        where
            parse _ capture xs (y : [])
                | y == '>' && capture   = (xs ++ [y], [])
                | otherwise             = (xs, [])
            parse _ _ xs []          = (xs, [])
            parse acc capture xs (x : y : ys)
                | x == '<' && y /= '/'  =  parse (acc + 1) True (xs ++ [x]) (y : ys)
                | x == '<' && y == '/'  =  parse (acc - 1) capture (xs ++ [x]) (y : ys)
                | y == '>' && acc == 0  = ((xs ++ [x] ++ [y]) , ys)
                | otherwise             =  
                    if      capture
                    then    parse acc capture (xs ++ [x]) (y : ys)
                    else    parse acc capture xs (y : ys)


    -- Get all the elements in the given content as a list.
    getElementList :: String -> [String]
    getElementList [] = []
    getElementList s = 
                let
                    (match, rest) = getNextElement s
                in
                    match : getElementList rest

    -- Get the contents of an element.
    getElementContent :: String -> String
    getElementContent [] = ""
    getElementContent s =  drop (length begin) $ take ((length s) - (length end)) s
        where
            begin =  openTag $  s
            end   =  closeTag $  s


    -- Get the opening tag of this element.
    getOpeningTag :: String -> String
    getOpeningTag [] = ""
    getOpeningTag s = parse 0 s
        where
            parse _ [] = []
            parse acc (x : xs)
                | x == '<'    = parse (acc + 1) xs
                | x == '>'    = []
                | otherwise   = 
                    if      acc > 0
                    then    x : parse acc xs
                    else    parse acc xs

    -- Get the name of the first element we encounter.
    getElementName :: String -> String
    getElementName [] = ""
    getElementName s = parse False s
        where
            parse _ [] = []
            parse capture (x : xs)
                | x == '<'     = parse True xs
                | x == ' ' || x == '>'     = 
                    if      capture
                    then    []
                    else    parse capture xs
                | otherwise     = 
                    if      capture
                    then    x : parse capture xs
                    else    parse capture xs



    -- Get the text nodes in the current element's content.
    getTextNodes :: String -> String
    getTextNodes [] = ""
    getTextNodes s = parse 0 True s
        where
            parse _ _ (y : [])
                | y == '>'      = [] 
                | otherwise     = [y]
            parse acc capture (x : y : xs)
                | x == '<' && y /= '/' = (parse (acc + 1) False (y : xs))
                | x == '<' && y == '/' = (parse (acc - 1) False (y : xs))
                | x == '>' && acc == 0 = (parse acc True (y : xs))
                | otherwise            = 
                    if      capture
                    then    x : (parse acc capture (y : xs))
                    else    parse acc capture (y : xs)



    -- Get a list of attribute name pairs
    -- This doesn't look good.
    getAttributeList :: String -> [String]
    getAttributeList [] = []
    getAttributeList s = march s'
        where
            s' = (getOpeningTag s) \\ (getElementName s)
            march [] = []
            march xs = match : march rest
                where
                    (match, rest) = getAttr xs
                        where
                            getAttr :: String -> (String, String)
                            getAttr s = parse 0 False [] s
                                where
                                    parse _ _ ys [] = (ys, [])
                                    parse acc capture ys (x : xs)
                                        | isAlpha x && not(capture)  = (parse acc True (ys ++ [x]) xs)
                                        | x =='\"' =
                                            if     acc == 1
                                            then   ((ys ++ [x]), xs)
                                            else   (parse (acc + 1) capture (ys ++ [x]) xs)
                                        | otherwise =
                                            if      capture
                                            then    (parse acc capture (ys ++ [x]) xs)
                                            else    parse acc capture (ys ++ [x]) xs



    -- gives the close and open tags of a specified element.
    closeTag :: String -> String
    closeTag [] = ""
    closeTag s = "</" ++ (getElementName s) ++ ">"

    openTag :: String -> String
    openTag [] = ""
    openTag s = "<" ++ (getOpeningTag s) ++ ">"

    -- Get all the name pair values from an element.
    -- Uses getAttributeList to create a list of tuples.
    getAttributes :: String -> [Attribute]
    getAttributes [] = []
    getAttributes s = res
        where
            s' = getAttributeList s
            res = map clean $ map (break (=='=')) s'
                where 
                    clean (a, b) = (removeTrailing a, removeTrailing $ tail b)

    -- Remove XML directives.
    -- e.g. <?xml version = "1.0" ...
    removeDirective :: String -> String
    removeDirective [] = ""
    removeDirective s  = 
        if      "<?" `isInfixOf` s
        then    removeDirective $ clean' s
        else    s
        where
            clean' (x : y : xs)
                | x == '?' && y == '>'  = xs
                | otherwise             = clean' (y : xs)
            clean' _ = ""


    -- Remove trailing whitespace.
    removeTrailing :: String -> String
    removeTrailing [] = ""
    removeTrailing s = march 0 s
        where
            march _ [] = ""
            march acc (x:xs)
                | x == '\"'  = 
                    if acc == 0
                    then x : (march 1 xs)
                    else x : (march 0 xs)
                | x == ' ' =
                    if acc == 0
                    then march acc xs
                    else x : (march acc xs)
                | otherwise = x : (march acc xs)

    -- Takes a string of XML, cleans it of tabs and newlines, and parses it into a
    -- Haskell data type.
    toXML :: String -> XMLValue
    toXML [] = XMLString ""
    toXML s = toXMLUnclean $ clean s
        where
            clean s = removeDirective $ filter (/='\t') $ unwords $ lines s


    testXML = "<school>yah<student id=\"1\"><name>Stephen</name></student><student id=\"2\"><name>Mona</name></student><shulman/></school>"

    -- Take a string containing XML and convert it to Haskell datatypes.
    toXMLUnclean :: String -> XMLValue
    toXMLUnclean [] = XMLString ""
    toXMLUnclean xs @ ( x : xs')
        | x == '<'  = 
            let
                elemName = getElementName xs        -- get the name of this element.
                attribs  = getAttributes xs         -- get the list of it's attributes.
                content  = getElementContent xs     -- get everything inside its tags.
                text     = getTextNodes content     -- get all its direct text node children.
                elemList = getElementList content   -- get a list of it's direct child elements.
                getRest  = 
                    -- If it doesn't have children elements..
                    -- Just return it's text node.
                    -- Otherwise, return it's children and it's text nodes.
                    if      all null elemList
                    then    [XMLString text]
                    else    (map toXMLUnclean elemList) ++ [XMLString (dropWhile isSpace text)]
            in
                -- If it's a self closing tag, then return it as an XMLClose, and dont recurse
                if      '/' `elem` elemName
                then     XMLClose (init elemName) attribs
                else     XMLElement elemName attribs getRest
        | x == ' '      = toXMLUnclean $ dropWhile isSpace xs
        | otherwise     = XMLString ""


    -- Take an XMLValue and output it as a JSON object.
    -- Couldn't decide on how to layout this information without losing data.
    -- For instance, where to put text nodes in elements that contain child elements and text..
    
--
--    toJSON :: XMLValue -> String
--    toJSON (XMLString s) = "\"" ++ s ++ "\""
--    toJSON (XMLClose n xs) = " { \"" ++ n ++ "\" : " ++ (toNamePair xs) ++ " } "
--    toJSON (XMLElement n xs c) = " \n{\n \"" ++ n ++ "\" : " ++ (concat $ map toJSON c) ++ " \n} "
--
--    -- Combine an attribute to form a name value pair in JSON format.
--    toNamePair :: [Attribute] -> String
--    toNamePair xs = foldl step [] xs
--        where
--            step rest (name, value) = rest ++ " { \"" ++ name ++ "\" : \"" ++ value ++ "\" } "

    
