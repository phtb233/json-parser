module XMLParser where

    import XML

    -- Parses an XML file at the specified path into the Haskell data type
    -- XMLValue.
    parseXML :: FilePath -> IO (XMLValue)
    parseXML s =
        do 
            xmlString <- readFile s
            return (toXML xmlString)