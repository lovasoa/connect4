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

--play::Grid->Color->Int->Grid
--play g color numCol = zip 

initial::Grid
initial = replicate 7 (replicate 6 Empty)

main = putStr $ showGrid $ map (addToken Red) initial
