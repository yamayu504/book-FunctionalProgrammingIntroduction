import Data.IntMap.Merge.Strict (zipWithMatched)
take' :: Int -> [b] -> [b]
take' 0 [b] = []
take' _ [] = []
take' a (x:xs) = x : take' (a-1)xs
   
drop' :: Int -> [b] -> [b]
drop' 0 [b] = [b]
drop' _ [] = []
drop' a (_:xs) = drop' (a-1) xs

ins :: Ord a => a -> [a] -> [a]
ins e [] = [e]
ins e (x:xs)
    | e < x      = e : x :xs
    | otherwise  = x : ins e xs

insSort :: Ord a => [a]->[a]
insSort [] = []
insSort (x:xs) = ins x ( insSort xs)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = [] 
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

zip' :: [a] -> [b] ->[(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys   