
import Data.List

splitIn [] x = []
splitIn [_] x = []
splitIn [_,_] x = []
splitIn list x = take x list : splitIn (tail list) x

searchGenome file match = map (+1) (findIndices (==match) (splitIn file (length match)))

main = do file <- readFile "data.txt"
          let list = head (lines file)
          let match = last (lines file)
          let result = searchGenome list match
          let print = foldl (\x y -> x ++ " " ++ (show y)) "" (result)
          putStrLn $ print
