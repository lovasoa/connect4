import Data.List

data Color = Red | Orange deriving (Eq)
data Cell  = Empty | Full(Color) deriving (Eq)

instance Show Color where
  show Red = " R "
  show Orange = " O "

instance Show Cell where
  show Empty   = " E "
  show (Full c) = show c

type Column = [Cell]
type Grid   = [Column]

showGrid:: Grid -> String
showGrid g = unlines $ map (concatMap show) (transpose g)

addToken:: Color -> Column -> Column
addToken color column = let (empties,fulls) = span (==Empty) column in
                          tail (empties ++ [Full color] ++ fulls) 

play::Grid->Color->Int->Grid
play g color numCol = let (prev,nexts) = splitAt numCol g in
                        prev ++ [addToken color (head nexts)] ++ (tail nexts)

summarize::Column->[(Cell,Int)]
summarize []  = []
summarize col = let (firsts,lasts) = span (==(head col)) col in
                  (head col, length firsts) : summarize lasts

size:: Grid -> (Int, Int)
size g = (length g, length $ head g)

enumerate:: Grid -> [((Int, Int), Cell)]
enumerate g = concat $ map (\(n, li) -> map (\(m, x) -> ((n,m),x)) (zip [0..] li)) (zip [0..] g)

getDiagonal::Grid->Int->Column
getDiagonal g numDiag = map snd $ filter (\((l,c),v) -> l+c == numDiag) (enumerate g)

getDiagonals:: Grid->Grid
getDiagonals grid = takeWhile (not.null) (map (getDiagonal grid) [0..])


initial::Grid
initial = replicate 7 (replicate 6 Empty)

main = print $ getDiagonals $ (play initial Red 0)
