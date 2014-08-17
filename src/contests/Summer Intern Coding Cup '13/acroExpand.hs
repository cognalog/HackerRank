import qualified Data.Char as C
import qualified Data.List as L

wordByWord :: ([[String]], Int) -> String -> ([[String]], Int)
wordByWord (xs, -1) word@(y:ys)
	| C.isUpper y && word /= "The" = ((word:[]):xs, 0)
	| otherwise = (xs, -1)
wordByWord (x:xs, n) word@(y:ys)
	| C.isUpper y = ((word:x):xs, 0)
        | (not . C.isLower) y = (x:xs, -1)
	| n > 0 = ((dropWhile (not . C.isUpper . head) x):xs, -1)
	| otherwise = ((word:x):xs, n + 1)

acroGet :: String -> [[String]]
acroGet para = 
	let arr = words para	
        in filter (\x -> (length x) > 1) . map reverse . fst $ foldl wordByWord ([], -1) arr
           
compHelper :: String -> [String] -> ([String], Bool)
compHelper t g = let caps = filter (C.isUpper . head) g
                 in (g, (length caps == length t) && (and $ zipWith (\tc gw -> tc == head gw) t caps))
           
acroComp :: [[String]] -> String -> String
acroComp gs t = let res = dropWhile (not . snd) $ map (compHelper t) gs
                in if null res then "Nothing"
                   else L.intercalate " " . fst $ head res
           
main = do
  n <- getLine
  cands <- fmap concat $ mapM (fmap acroGet) $ (take $ read n) . repeat $ getLine
  tests <- sequence $ (take $ read n) . repeat $ getLine
  sequence $ map (putStrLn . id) $ map (acroComp cands) tests