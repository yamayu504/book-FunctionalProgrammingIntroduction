module DoctestSample where 

-- |文字列中のスペースの個数
-- 
-- >>> countSpace ""
-- 0
-- >>> countSpace "  "
-- 2

-- prop> countSpace s == sum[1 | c <-s, c == ' ']

countSpace :: String -> Int
countSpace = length . filter (' ' ==)