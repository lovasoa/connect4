data Color = Red | Orange
data Cell  = Empty | Full(Color)

type Column = [Cell]
type Grid   = [Column]

initial::Grid
initial = replicate 7 (replicate 6 Empty)

main = print "Hello"
