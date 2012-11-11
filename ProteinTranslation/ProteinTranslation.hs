import Data.List

translate (a)
  | a == "UUU" = "F"
  | a == "CUU" = "L"      
  | a == "AUU" = "I"      
  | a == "GUU" = "V"
  | a == "UUC" = "F"
  | a == "CUC" = "L"  
  | a == "AUC" = "I" 
  | a == "GUC" = "V"
  | a == "UUA" = "L" 
  | a == "CUA" = "L"   
  | a == "AUA" = "I" 
  | a == "GUA" = "V"
  | a == "UUG" = "L" 
  | a == "CUG" = "L"   
  | a == "AUG" = "M" 
  | a == "GUG" = "V"
  | a == "UCU" = "S"   
  | a == "CCU" = "P"   
  | a == "ACU" = "T"  
  | a == "GCU" = "A"
  | a == "UCC" = "S" 
  | a == "CCC" = "P"  
  | a == "ACC" = "T"  
  | a == "GCC" = "A"
  | a == "UCA" = "S"  
  | a == "CCA" = "P"  
  | a == "ACA" = "T"  
  | a == "GCA" = "A"
  | a == "UCG" = "S"   
  | a == "CCG" = "P"   
  | a == "ACG" = "T"  
  | a == "GCG" = "A"
  | a == "UAU" = "Y"   
  | a == "CAU" = "H"  
  | a == "AAU" = "N"  
  | a == "GAU" = "D"
  | a == "UAC" = "Y"  
  | a == "CAC" = "H"  
  | a == "AAC" = "N"  
  | a == "GAC" = "D"
  | a == "UAA" = "Stop"  
  | a == "CAA" = "Q"   
  | a == "AAA" = "K"  
  | a == "GAA" = "E"
  | a == "UAG" = "Stop" 
  | a == "CAG" = "Q"   
  | a == "AAG" = "K"  
  | a == "GAG" = "E"
  | a == "UGU" = "C"    
  | a == "CGU" = "R"  
  | a == "AGU" = "S"  
  | a == "GGU" = "G"
  | a == "UGC" = "C"    
  | a == "CGC" = "R"  
  | a == "AGC" = "S"   
  | a == "GGC" = "G"
  | a == "UGA" = "Stop" 
  | a == "CGA" = "R"  
  | a == "AGA" = "R"   
  | a == "GGA" = "G"
  | a == "UGG" = "W"   
  | a == "CGG" = "R"  
  | a == "AGG" = "R"  
  | a == "GGG" = "G" 
  | otherwise  =  a

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

cutList list =
  case (findIndex (== "Stop") list) of
    Just a  -> take a list
    Nothing -> list

translateToProtein list =  foldl (++) [] (cutList (map (translate) (splitEvery 3 list)))


main = do 
          file <- readFile "data.txt"
          putStrLn $ translateToProtein file 
