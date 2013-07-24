import qualified Data.Char as C
import qualified Data.List as L

wordByWord :: ([[[Char]]], Int) -> [Char] -> ([[[Char]]], Int)
wordByWord (xs, -1) word@(y:ys)
	| C.isUpper y && word /= "The" = ((word:[]):xs, 0)
	| otherwise = (xs, -1)
wordByWord (x:xs, n) word@(y:ys)
	| C.isUpper y = ((word:x):xs, 0)
        | (not . C.isLower) y = (x:xs, -1)
	| n > 0 = ((dropWhile (not . C.isUpper . head) x):xs, -1)
	| otherwise = ((word:x):xs, n + 1)

acroGet :: [Char] -> [[[Char]]]
acroGet para = 
	let arr = words para	
        in filter (\x -> (length x) > 1) . map reverse . fst $ foldl wordByWord ([], -1) arr
           
compHelper :: [Char] -> [[Char]] -> ([[Char]], Bool)
compHelper t g = let caps = filter (C.isUpper . head) g
                 in (g, (length caps == length t) && (and $ zipWith (\tc gw -> tc == head gw) t caps))
           
acroComp :: [[[Char]]] -> [Char] -> [Char]
acroComp gs t = let res = takeWhile (snd) $ map (compHelper t) gs
                in if null res then "Nothing"
                   else L.intercalate " " . fst $ head res
           
main = do
  n <- getLine
  cands <- fmap concat $ mapM (fmap acroGet) $ (take $ read n) . repeat $ getLine
  tests <- sequence $ (take $ read n) . repeat $ getLine
  print $ map (acroComp cands) tests