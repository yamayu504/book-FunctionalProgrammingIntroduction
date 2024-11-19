module RLE where
import Data.List (group)

-- | ラングレス圧縮
-- | test
-- 
-- >>> rle "AA"
-- "A2"
-- >>> rle "AAABBCCCA"
-- "A3B2C3A1"
-- >>> rle "AA"
-- ""
-- 



rle :: String -> String
rle = fromCharAndRunLength .toCharAndRunLength

toCharAndRunLength :: [Char] -> [(Char, Int)]
toCharAndRunLength = torls . group

torls :: [[Char]] ->  [(Char, Int)]
torls = map (\ x -> (head x, length x))

fromCharAndRunLength :: [(Char, Int)] -> String
fromCharAndRunLength = concatMap rls2strs'

rls2strs :: [(Char, Int)] -> [String]
rls2strs [] = []
rls2strs ((a, b):xs)  =  (a :show b) : rls2strs xs

rls2strs' :: (Char, Int) -> String
rls2strs' (a,b) = a : show b


rle2 :: String -> String
rle2 = concatMap (rls2strs' . toPair) . group

toPair :: [Char] -> (Char, Int)
toPair xs = (head xs,length xs)