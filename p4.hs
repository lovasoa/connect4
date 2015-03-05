data Color = Red | Orange
data Cell  = Empty | Full(Color)

instance Show Color where
  show Red = " R "
  show Orange = " O "

instance Show Cell where
  show Empty   = "   "
  show (Full c) = show c

type Column = [Cell]
type Grid   = [Column]

initial::Grid
initial = replicate 7 (replicate 6 Empty)

main = print (Empty)
