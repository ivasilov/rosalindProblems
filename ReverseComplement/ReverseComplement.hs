reverseDNA :: Char -> Char
reverseDNA c
   | c == 'A' = 'T'
   | c == 'C' = 'G'
   | c == 'G' = 'C'
   | c == 'T' = 'A'
   | otherwise = c

main = do file <- readFile "data.txt"
          let genome = map (reverseDNA) (reverse file)
          putStr genome
