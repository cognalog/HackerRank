import qualified Data.Char as Char

wordByWord :: (Num b, Show a) => ([[[a]]], b) -> [a] -> ([[[a]]], b)
wordByWord (xs, -1) y:ys
	| Char.isUpper (head y) = ((y:[]):xs, 0)
	| otherwise = (xs, -1)
wordByWord (x:xs, n) y:ys
	| Char.isUpper (head y) = ((y:x):xs, 0)
	| n > 2 = (x:xs, -1)
	| otherwise = ((y:x):xs, n + 1)

getAcronyms :: (Show s) => [s] -> [[[s]]]
getAcronyms para = 
	let arr = words para	
	in 	head $ foldl wordByWord ([], -1) arr

main = do
	pg <- getLine
	putStrLn getAcronyms pg