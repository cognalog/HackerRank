import qualified Data.Map as Map

charCounts :: (Ord a, Num i) => [a] -> Map.Map a i
charCounts = foldr (\a acc -> Map.insertWith (+) a 1 $ acc) Map.empty

yn :: Bool -> String
yn True = "YES"
yn False = "NO"

palinagram :: (Ord a) => [a] -> IO ()
palinagram xs = putStr $ yn . (<=1) . Map.size . Map.filter odd $ charCounts xs

main = do
  name <- getLine
  palinagram name