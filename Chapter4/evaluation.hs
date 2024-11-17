module Evaluatiion where

nats :: [Integer]
nats = 0 : map (+1) nats

-- フィボナッチ数列は、自分の前の２つの数字を足すので、
-- 数値をずらした数列を足した(zipWith)した配列と後半はおなじになる。
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

data Tree a = Leaf {element :: a}
            | Fork {element :: a
                ,left  :: Tree a
                ,right :: Tree a
                } deriving Show

mean' :: [Double] -> Double
mean' xs = 
    let (res, len) = foldl (\(m,n) x -> (m + x / len, n + 1)) (0,0) xs 
    in res