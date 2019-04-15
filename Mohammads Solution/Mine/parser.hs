module Main
where
	import Mine
	import Data.Char
	import System.IO
	import Data.List
	import Text.Parsec
	import Text.Parsec.String

	isbrBracket :: Char -> Bool
	isbrBracket c = not( c == '/')

	--myparse :: -> String -> JV
	myparse k = parse parseJValue "test"

	remove :: String -> String
	remove [] = []
	remove (x:xs)
		| x == '{' || x=='}' || x==',' || x==':'	= remove xs
	    | otherwise 								= x : remove xs 



	 
	oo :: FilePath -> IO ()
	oo s = do  
		str <-  readFile s
		let j = map (filter (/='\r')) $ lines $  tail $ init $ str 
		let n = map remove j
		let y = myparse n
		print(j)
