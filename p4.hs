import Data.List

data Color = Red | Orange
data Cell  = Empty | Full(Color)

instance Show Color where
  show Red = " R "
  show Orange = " O "

instance Show Cell where
  show Empty   = " E "
  show (Full c) = show c

type Column = [Cell]
type Grid   = [Column]

showGrid:: Grid -> String
showGrid g = tail $ concatMap (('\n' :) . (concatMap show)) (transpose g)

initial::Grid
initial = replicate 7 (replicate 6 Empty)

main = putStr $ showGrid initial
