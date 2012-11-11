
transcribe :: Char -> Char
transcribe c
   | c == 'T' = 'U'
   | otherwise = c

main = do file <- readFile "data.txt"
          let genome = map (transcribe) file
          putStr genome
