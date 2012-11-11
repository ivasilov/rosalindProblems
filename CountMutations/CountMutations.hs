import Data.List

main = do 
          file <- readFile "data.txt"
          let dataLines = lines file
          putStrLn $ show $ length (filter (\ (x,y) -> x /= y) (zip (dataLines !! 0) (dataLines !! 1)))
