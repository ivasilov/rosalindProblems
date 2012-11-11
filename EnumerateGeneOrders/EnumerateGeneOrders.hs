import Data.List

main = do file <- readFile "data.txt"
          let combinations = permutations ['1' .. head file]
          putStrLn $ show $ length combinations
          mapM (\x -> putStrLn $ intersperse ' ' x) combinations
