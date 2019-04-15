-- Read in XML and store it in Haskell data type.

module XML(toXML, toJSON, XMLValue(..)) where


    import Debug.Trace(trace)
    import Data.Char(isAlpha, isSpace)
    import Data.List((\\), isInfixOf, intersperse)

    type Name       = String
    type Value      = String
    type Attribute  = (Name, Value)
    type Contents   = [XMLValue]          

   
    data XMLValue =
        XMLString  String
      | XMLElement Name [Attribute] Contents
      | XMLClose   Name [Attribute]             -- <element/> Self closing elements.
      deriving (Show, Eq, Ord, Read)


    -- Get the tags and content of the first outermost element (and the rest of the
    -- string) in a tuple (match, rest).
    -- Most of the ugliness in the function body (as with the rest of these functions) comes
    -- from having to step over strings/inner elements that will confuse the matches. 
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


    -- Get the opening tag of this element, including attributes.
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


    -- Get the name of the outermost element.
    -- e.g. "<foo id=\"123\">"  =   "foo"
    getElementName :: String -> String
    getElementName = takeWhile (/= ' ') .tail . fst . 
                     break (=='>') . snd . break (=='<')



    -- Get the text nodes in the current element's content.
    -- e.g. "text stuff<bar>inner things</bar> aswell" = "text stuff aswell"
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
    -- Takes the output of calls to getAttr and orders them in a list.
    getAttributeList :: String -> [String]
    getAttributeList [] = []
    getAttributeList s = march s'
        where
        -- Remove the element name from the input.
        s' = (getOpeningTag s) \\ (getElementName s)
        march [] = []
        march xs = match : march rest
            where
            (match, rest) = getAttr xs
                where
                -- Breaks it's input into a pair, after escaped quotation marks have opened and closed.
                -- e.g. "<foo id=\"wind\" num = \"break\">" = ("<foo id=\"wind\", "num = \"break\">")
                getAttr :: String -> (String, String)
                getAttr s = parse False [] s
                    where
                    parse _ ys [] = (ys, [])
                    parse inString ys (x : xs)
                        | isAlpha x     = parse inString (ys ++ [x]) xs
                        | x =='\"' =
                            if     inString
                            then   ((ys ++ [x]), xs)
                            else   parse True (ys ++ [x]) xs
                        | otherwise     = parse inString (ys ++ [x]) xs



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
    removeTrailing = reverse . dropWhile isSpace . 
                     reverse . dropWhile isSpace

    -- Takes a string of XML, cleans it of tabs and newlines, and parses it into a
    -- Haskell data type. Takes into consideration carriage returns '\r' when used in a 
    -- POSIX environment.
    toXML :: String -> XMLValue
    toXML [] = XMLString ""
    toXML s = toXMLUnclean $ clean s
        where
            clean s = removeDirective $ filter noReturnTab $ unwords $ lines s
                where 
                    noReturnTab c = c /= '\t' && c/= '\r'


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
                    else    (map toXMLUnclean elemList) -- ++ [XMLString (dropWhile isSpace text)]
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
    -- Where elements have both text and child nodes, the text is omitted.

    toJSON :: XMLValue -> String
    toJSON j = toJSON' j 0
        where
            toJSON' (XMLString s) ind                  = "\"" ++ s ++ "\""
            toJSON' jo@(XMLElement n attr child) ind   = "\n" ++ (xmlToObject jo ind)
            toJSON' jo@(XMLClose n attr)         ind      = "\n" ++ (xmlToObject jo ind)
            -- Turn an Haskell XMLElement into a JSON object.
            xmlToObject :: XMLValue -> Int -> String
            xmlToObject (XMLElement name xs content) ind = 
                    (room ind)        ++ " { \n" ++ 
                    (room (ind + 4) ) ++ "\"" ++ name ++ "\"" ++ " : \n" ++
                    (room (ind + 4) ) ++ "[\n" ++
                    (room (ind + 8) ) ++ " { \"attributes\" : [" ++ (concat $ intersperse "," (map attrToObject xs)) ++ "] } ,\n" ++
                    (room (ind + 8) ) ++ " { \"children\" : [" ++ (concat $ intersperse (room(ind + 4) ++ ",") (filter (/="\"\"") $ map (toJSON_(ind + 8))  content)) ++ (isElement (head content) ind) ++ "]} \n" ++
                    --(room (ind + 8) ) ++ " { text : " ++ (isText $ last content) ++ " }\n" ++
                    (room (ind + 4) ) ++ "] \n" ++
                    (room ind)        ++ " }\n "
            xmlToObject (XMLClose name xs) ind =
                (room ind)        ++ " { \n" ++ 
                (room (ind + 4) ) ++ "\"" ++ name ++ "\"" ++ " : \n" ++
                (room (ind + 4) ) ++ "[\n" ++
                (room (ind + 8) ) ++ " { \"attributes\" : [" ++ (concat $ intersperse "," (map attrToObject xs)) ++ "] } \n" ++
                (room (ind + 4) ) ++ "] \n" ++
                        (room ind)        ++ " }\n "
            -- Just flip the arguments of toJSON' around.
            toJSON_ :: Int -> XMLValue -> String
            toJSON_ = flip toJSON'
            -- Create indents.
            room :: Int -> String
            room i = replicate i ' '   
            -- Turn attribute name pairs into one JObject.           
            attrToObject :: (String, String) -> String
            attrToObject (name, value) = " { \"" ++ name ++ "\": " ++ value ++ " } "
            -- Only indent if the XMLValue is an XMLElement.
            isElement :: XMLValue -> Int -> String
            isElement (XMLElement _ _ _) n = room(n + 4)
            isElement _ _                  = ""
            isText :: XMLValue -> String
            isText (XMLString s) = "\"" ++ s ++ "\""
            isText _             = "null"
        
