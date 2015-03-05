import Data.List
import Data.Maybe
import Data.Function

wonAt = 4

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
type Summary = [(Cell, Int)]

otherColor:: Color -> Color
otherColor Red = Orange
otherColor Orange = Red

showGrid:: Grid -> String
showGrid g = unlines $ map (concatMap show) (transpose g)

addToken:: Color -> Column -> Column
addToken color column = let (empties,fulls) = span (==Empty) column in
                          tail (empties ++ [Full color] ++ fulls) 

play::Color->Int->Grid->Grid
play color numCol g = let (prev,nexts) = splitAt numCol g in
                        prev ++ [addToken color (head nexts)] ++ (tail nexts)

summarize::Column->Summary
summarize []  = []
summarize col = let (firsts,lasts) = span (==(head col)) col in
                  (head col, length firsts) : summarize lasts

summarizeGrid::Grid->Summary
summarizeGrid grid = concatMap (\f -> concatMap summarize (f grid))
                      [getDiagonals, getDiagonals.(map reverse), id, transpose]

size:: Grid -> (Int, Int)
size g = (length g, length $ head g)

getDiagonal::Grid->Int->Column
getDiagonal grid numDiag = [grid!!i!!j |
                              i<-[0..(fst$size grid)-1], j<-[0..(snd$size grid)-1],
                              i+j==numDiag]

getDiagonals:: Grid->Grid
getDiagonals grid = takeWhile (not.null) (map (getDiagonal grid) [0..])

won:: Grid -> Maybe Color
won = listToMaybe.
            (map (\(Full x,n) -> x)).
            (filter (\(v,num) -> v/=Empty && num>=wonAt)).
            summarizeGrid

legalMoves::Grid -> [Int]
legalMoves g = map fst (filter ((==Empty).head.snd) (zip [0..] g))

evaluate:: Grid -> Int
evaluate = sum.(map (\(c,num) -> 
                100 ^ num * case c of
                  Full Red -> 1
                  Full Orange -> -1
                  Empty -> 0
            )).summarizeGrid

negaMax::Color->Int->Int->Grid->(Int,Int)
negaMax color depthMax depth grid | depthMax == depth =
  let evaluation = (if color==Red then 1 else -1) * (evaluate grid) in
    (evaluation, 0)

negaMax color depthMax depth grid =
  let nextCol = otherColor color
      playit  = flip (play color) grid
  in
    maximumBy (compare `on` fst) 
      (map (\move ->
              let negMaxVal = fst$(negaMax nextCol depthMax (depth+1))$playit move in
              (-1 * negMaxVal, move))
          (legalMoves grid))

initial::Grid
initial = replicate 7 (replicate 6 Empty)

main = let playr0 = play Red 2 in
          print $ evaluate $ (foldr (.) id (replicate 5 playr0)) initial
