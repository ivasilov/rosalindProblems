import Data.List
import Data.Map

calcPercentage :: Fractional a => [Char] -> a
calcPercentage c = ((Data.List.genericLength (Prelude.filter (\x -> x == 'C' || x == 'G') c)) / (Data.List.genericLength c)) * 100

testRosalind ('>':xs) = True
testRosalind x = False 

joinStrings Nothing y = y
joinStrings (Just x) y = x ++ y  

insertWholeFile container [] _ = container
insertWholeFile container (x:xs) key =  if (testRosalind x) 
                                          then 
                                            insertWholeFile container xs (tail x)
                                          else
                                            insertWholeFile (Data.Map.insert key (joinStrings (Data.Map.lookup key container) x) container) xs key

comparePercentage (a,a1) (b,b1)
  | calcPercentage a1 > calcPercentage b1 = LT
  | calcPercentage a1 < calcPercentage b1 = GT
  | calcPercentage a1 == calcPercentage b1 = GT

main = do 
          file <- readFile "data.txt"
          let dataMap = (insertWholeFile (fromList []) (lines file) "") 
          let topValue = head (sortBy comparePercentage (toList dataMap))
          putStrLn $ (fst topValue)
          putStrLn $ show $ calcPercentage (snd topValue)
