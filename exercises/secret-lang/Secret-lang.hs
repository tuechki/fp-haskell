import Data.Char(toLower)


isLetter :: Char -> Bool
isLetter char 
	| toLower char >= 'a' 
	  && toLower char <= 'z' = True
	|otherwise = False


isVowel :: Char -> Bool
isVowel letter | toLower letter == 'a' = True
	       | toLower letter == 'e' = True
	       | toLower letter == 'i' = True
	       | toLower letter == 'o' = True
	       | toLower letter == 'u' = True
	       | toLower letter == 'y' = True
	       | otherwise = False


isConsonant :: Char -> Bool
isConsonant letter | isLetter letter == True 
		     && isVowel letter == False = True 
		   | otherwise = False

emptyString = "" :: String


getReverseString :: String -> String -> String
getReverseString acc [] = acc
getReverseString acc (x:xs) = getReverseString (x : acc) xs


encodeLoop :: String -> String -> String
encodeLoop acc []     = acc
encodeLoop acc (x:xs) | isLetter x == True
		    && isConsonant x == True = encodeLoop (x : 'o' : x : acc) xs
		  | otherwise = encodeLoop (x : acc) xs 


encode :: String -> String
encode string = encodeLoop emptyString 
			     (getReverseString emptyString string)




-- Bonus #2
dropN :: Int -> String -> String
dropN = undefined


--Assume we'll be decoding only valid words
decode :: String -> String
decode [] = []
decode (first : second : third : rest) 
	| isConsonant first == True 
	  && second == 'o' 
	  && toLower first == toLower third = first : decode rest
	| otherwise = first : decode (second : third : rest)
decode (x:rest) = x : decode rest





