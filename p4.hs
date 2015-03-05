import Data.List
import Data.Maybe

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

showGrid:: Grid -> String
showGrid g = unlines $ map (concatMap show) (transpose g)

addToken:: Color -> Column -> Column
addToken color column = let (empties,fulls) = span (==Empty) column in
                          tail (empties ++ [Full color] ++ fulls) 

play::Color->Int->Grid->Grid
play color numCol g = let (prev,nexts) = splitAt numCol g in
                        prev ++ [addToken color (head nexts)] ++ (tail nexts)

summarize::Column->[(Cell,Int)]
summarize []  = []
summarize col = let (firsts,lasts) = span (==(head col)) col in
                  (head col, length firsts) : summarize lasts

size:: Grid -> (Int, Int)
size g = (length g, length $ head g)

getDiagonal::Grid->Int->Column
getDiagonal grid numDiag = [grid!!i!!j |
                              i<-[0..(fst$size grid)-1], j<-[0..(snd$size grid)-1],
                              i+j==numDiag]

getDiagonals:: Grid->Grid
getDiagonals grid = takeWhile (not.null) (map (getDiagonal grid) [0..])

columnWon:: Column -> Maybe Color
columnWon = listToMaybe.
            (map (\(Full x,n) -> x)).
            (filter (\(v,num) -> v/=Empty && num>=wonAt)).
            summarize

won:: Grid->Maybe Color
won grid = listToMaybe $ catMaybes $
          concatMap (\f -> map columnWon (f grid))
                    [getDiagonals, getDiagonals.(map reverse), id, transpose]


initial::Grid
initial = replicate 7 (replicate 6 Empty)

main = let playr0 = play Red 0 in
          print $ won $ (foldr (.) id (replicate 4 playr0)) initial
