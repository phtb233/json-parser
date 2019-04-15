module XMLParser where

    import XML

    -- ADDITIONAL TASK : Parse an XML file to JSON, and return it as a string.
    -- The processor instructions (xml version, xml schema, DTD etc.) are discarded.
    -- Where elements contain both text and child elements, the text is omitted.

    -- Parses an XML file at the specified path into the Haskell data type XMLValue.
    parseXML :: FilePath -> IO (XMLValue)
    parseXML s =
        do 
            xmlString <- readFile s
            return $ toXML xmlString

    -- Parses XML file at path, and returns a string in JSON format.
    parseXMLtoJSON :: FilePath -> IO(String)
    parseXMLtoJSON s =
        do
            xmlVal <- parseXML s
            return $ toJSON xmlVal