
-- 改善点
-- \\関数
-- list 糖衣構文+anyでor条件をとる。
-- 

import Data.List
import Data.Ord (comparing)
import Data.Function

type Cell = (Int, Int)
type Board = [(Cell,Int)]

solve :: Board -> [Board]
solve board | length board == 81 = [board]
solve board =
    [ (masu,suuchi) : board
        |let masuList = masuCandidate board,
         let masu = findCellWithMinValue (suchiLengthTuple board masuList),
        suuchi <- suuchiCandidate board masu
        ] >>= solve

findCellWithMinValue :: [(Cell, Int)] -> Cell
findCellWithMinValue = fst . minimumBy (comparing snd)

suchiLengthTuple :: Board -> [Cell] -> [(Cell, Int)]
suchiLengthTuple board = map (\x-> (x, length (suuchiCandidate board x)))


masuCandidate :: Board -> [Cell]
masuCandidate board = filter (\x-> x `notElem` map fst board ) [(x, y) | x <- [0..8], y <- [0..8]]

suuchiCandidate :: Board -> Cell -> [Int]
suuchiCandidate = leftCandidates

searchCellSuchi :: Board -> Cell -> Maybe Int
searchCellSuchi [] _ = Nothing -- 配列が空ならば、Nothing
searchCellSuchi ((a,b):xs) cell = if a == cell
    then Just b -- cellが一致すればIntを返す
    else searchCellSuchi xs cell   -- 一致するものがなければtailを検索

leftCandidates :: Board -> Cell -> [Int]
leftCandidates board cell =
   let fixNum = nub (verticalleftCandidate board cell ++ horizontalleftCandidate board cell ++ boxleftCandidate board cell)
   in filter (`notElem` fixNum) [1..9]

verticalleftCandidate :: Board -> Cell -> [Int]
verticalleftCandidate board (c,_) = map snd (filter (\((x, _), _) -> x == c) board)

horizontalleftCandidate :: Board -> Cell -> [Int]
horizontalleftCandidate board (_,c) = map snd (filter (\((_, x), _) -> x == c) board)

boxleftCandidate :: Board -> Cell -> [Int]
boxleftCandidate board cell =
    let cellboxNum = searchBox cell
    in map snd (filter (\(x, _) -> cellboxNum == searchBox x) board)

searchBox :: Cell -> Int
searchBox (a,b) =  b `div` 3 *3 + a `div` 3


main :: IO ()
main = case solve problem of
    answer : _ -> mapM_ print $ format answer
    []         -> putStrLn "invalid Problem"

format :: Board -> [[Int]]
format = map (map snd) . transpose . groupBy( (==) `on`(fst .fst)) .sort



problem :: Board
problem = [
    ((3,0),8),
    ((5,0),1),
    ((6,1),4),
    ((7,1),3),
    ((0,2),5),
    ((4,3),7),
    ((6,3),8),
    ((6,4),1),
    ((1,5),2),
    ((4,5),3),
    ((0,6),6),
    ((7,6),7),
    ((8,6),5),
    ((2,7),3),
    ((3,7),4),
    ((3,8),2),
    ((6,8),6)
    ]