import qualified Data.Char as C

wordByWord :: ([[[Char]]], Int) -> [Char] -> ([[[Char]]], Int)
wordByWord (xs, -1) word@(y:ys)
	| C.isUpper y && word /= "The" = ((word:[]):xs, 0)
	| otherwise = (xs, -1)
wordByWord (x:xs, n) word@(y:ys)
	| C.isUpper y = ((word:x):xs, 0)
        | (not . C.isLower) y = (x:xs, -1)
	| n > 0 = ((dropWhile (not . C.isUpper . head) x):xs, -1)
	| otherwise = ((word:x):xs, n + 1)

getAcronyms :: [Char] -> [[[Char]]]
getAcronyms para = 
	let arr = words para	
        in filter (\x -> (length x) > 1) . map reverse . fst $ foldl wordByWord ([], -1) arr
           
main = do
  putStrLn "Paragraph:"
  para <- getLine
  putStrLn $ show $ getAcronyms para