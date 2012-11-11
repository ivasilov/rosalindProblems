addList xs ys = map (\(x,y) -> x + y) (zip xs ys)

decode :: Char -> [Int]
decode c
   | c == 'A' = [1,0,0,0]
   | c == 'C' = [0,1,0,0]
   | c == 'G' = [0,0,1,0]
   | c == 'T' = [0,0,0,1]
   | otherwise = [0,0,0,0]

stringMapper :: [Char] -> [[Int]]
stringMapper input = map (decode) input

merger :: [Char] -> [Int]
merger input = foldl (addList) [0,0,0,0] (stringMapper input)

main = do file <- readFile "data.txt"
          let genome = foldl (\x y -> x ++ " " ++ show y) "" (merger file)
          putStr genome
