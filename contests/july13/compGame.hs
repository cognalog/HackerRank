data MarkNum = MarkNum Bool Int
instance Show MarkNum where
  show (MarkNum m n) = show n

getArrs :: (String, String) -> ([MarkNum], [MarkNum])
getArrs (a1, a2) = let arr1 = map read $ words a1
                       arr2 = map read $ words a2
                   in (map (MarkNum False) arr1, map (MarkNum False) arr2)

main = do
  n <- getLine
  a1 <- getLine
  a2 <- getLine
  putStr "done"