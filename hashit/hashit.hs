import Data.Char
import Data.List

h ks = 19 * (foldl (\acc elem -> acc + (ord (snd elem)) * (fst elem)) 0 (zip [1..15] ks))
hash key = h key

pos key j = (hash key + (j ^ 2) + (23 * j)) `mod` 101

add' key xs 19 = xs 
add' key xs j = let index = pos key j
                in if xs !! index == "" 
                  then (take index xs) ++ key : (drop (index + 1) xs) 
                else
                  if xs !! index /= key 
                  then add' key xs (j+1)
                  else xs

add key xs = add' key xs 0 

find' key xs 19 = -1 
find' key xs j = let index = pos key j 
                 in if (xs !! index) == key 
                    then index 
                 else
                    find' key xs (j+1)

findKey key xs = find' key xs 0 

del key xs = let index = findKey key xs
             in if index > -1 
             then (take index xs) ++ "" : (drop (index + 1) xs)
             else xs

handlecommand xs ('A' : 'D' : 'D' : ':' : key) = add key xs
handlecommand xs ('D' : 'E' : 'L' : ':' : key) = del key xs
handlecommand xs _ = xs

handlecommands xs = let hashed = foldl (\acc c -> handlecommand acc c) (replicate 101 "") xs 
                     in let ignoreempty = filter (\e -> snd e /= "" ) (zip [0..100] hashed)
                        in let no = length ignoreempty 
                           in let items = foldl (\acc c -> ((show (fst c)) ++ ":" ++ (snd c)):acc) [] ignoreempty  
                              in (show no):reverse items

allcompute ls 0 = ls
allcompute ls c = let nbentries = rInt (head ls)
                  in let result = handlecommands (take nbentries (tail ls))
                     in result ++ (allcompute (drop (nbentries+1) ls) (c - 1))

rInt :: String -> Int
rInt = read

main = do
  s <- getContents
  let l = lines s
  let cases = rInt (head l)
  let r = allcompute (tail l) cases
  putStr (unlines r)


