import Data.List
import Data.Maybe
import Data.Function
import Data.Char
import System.IO

import Debug.Trace

wonAt = 4

data Color = Red | Orange deriving (Eq)
data Cell  = Empty | Full(Color) deriving (Eq)

instance Show Color where
  show Red = " ◉ "
  show Orange = " ◎ "

instance Show Cell where
  show Empty   = " ⬜ "
  show (Full c) = show c

type Column = [Cell]
type Grid   = [Column]
type Summary = [(Cell, Int)]

otherColor:: Color -> Color
otherColor Red = Orange
otherColor Orange = Red

showGrid:: Grid -> String
showGrid g = unlines $ (map (concatMap show) (transpose g) ++ [showColNums g])

showColNums:: Grid -> String
showColNums grid = let legals = legalMoves grid in
                       concatMap (\x -> if x `elem` legals then " "++show x++" " else "   ")
                           [0..length grid]

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

-- Get the lines, columns and diagonals of a grid
allAlignments::Grid->[Column]
allAlignments grid = concatMap ($ grid)
                      [getDiagonals, getDiagonals.(map reverse), id, transpose]

size:: Grid -> (Int, Int)
size g = (length g, length $ head g)

getDiagonal::Grid->Int->Column
getDiagonal grid numDiag = [grid!!i!!j |
                              i<-[0..(fst$size grid)-1], j<-[0..(snd$size grid)-1],
                              i+j==numDiag]

getDiagonals:: Grid->Grid
getDiagonals grid = (takeWhile (not.null) (map (getDiagonal grid) [0..]))

won:: Grid -> Maybe Color
won = listToMaybe.
            (map (\(Full x,n) -> x)).
            (filter (\(v,num) -> v/=Empty && num>=wonAt)).
            summarizeGrid

legalMoves::Grid -> [Int]
legalMoves g = map fst (filter ((==Empty).head.snd) (zip [0..] g))

-- Evaluates how advantageous the grid is for the Red player
evaluate:: Grid -> Int
evaluate = sum.(map evaluateColumn).allAlignments

evaluateColumn:: Column -> Int
evaluateColumn col
      | length col >= wonAt = (evaluate4 $ take wonAt col) + (evaluateColumn $ tail col)
      | otherwise = 0

-- Evaluates how profitable a set of 4 cells is for the Red
evaluate4::[Cell]->Int
evaluate4 cells = case evaluate4orLess cells Nothing 0 of
                      (Nothing, _) -> 0
                      (Just c, s) -> 10^s * if c==Red then 1 else -1

evaluate4orLess::[Cell]->(Maybe Color)->Int->(Maybe Color, Int)
evaluate4orLess (Full cellColor:cells) color curSum
  | color == Just cellColor || color == Nothing
    = evaluate4orLess cells (Just cellColor) (curSum+1)
evaluate4orLess (Empty:cells) color curSum = evaluate4orLess cells color curSum
evaluate4orLess [] color sum = (color, sum)
evaluate4orLess _ _ _ = (Nothing, 0) -- There are two different colors in the 4 cells

-- Returns a tuple.
--  - The first element represents a move (0-6)
--  - The second represents how advantageous that move is for the given color
--  depth represents the number of moves to foresee
negaMax::Color->Int->Grid -> (Int, Int)
negaMax color depth grid | depth == 0 || (isJust $ won grid) =
  let evaluation = (if color==Red then 1 else -1) * (evaluate grid) in
    (0, evaluation)

negaMax color depth grid =
  let nextCol = otherColor color
      playit  = flip (play color) grid
  in
    maximumBy (compare `on` snd)
                        (map
                          (\m -> (m, -(snd$negaMax nextCol (depth-1) (playit m))))
                          (legalMoves grid))
aimove::Color->Grid->Int
aimove color grid = fst $ negaMax color 5 grid

initial::Grid
initial = replicate 7 (replicate 6 Empty)

-- Un joueur générique
class Contestant a where
  move :: a -> Grid -> IO Int -- donner un coup à jouer
  color :: a -> Color         -- donner sa couleur

data Human = Human Color
data Computer = Computer Color

instance Contestant Computer where
  move  (Computer col) grid = return$aimove col grid
  color (Computer col) = col

instance Contestant Human where
  move hum grid = do
    moveChar <- getChar
    let moveInt = digitToInt moveChar in
      if moveInt `elem` (legalMoves grid) then
        return moveInt
      else do
        putStrLn "Wrong move!"
        move hum grid

  color (Human col) = col


loop::(Contestant a,Contestant b)=>Grid->a->b->IO()
loop grid a b = do
  if null $ legalMoves grid then
    putStrLn "Ex-aequo"
  else do
    putStrLn $ showGrid grid
    amove <- move a grid
    let newgrid = play (color a) amove grid in do
      case won newgrid of
        Just color -> putStrLn (showGrid newgrid ++ "\n" ++ show color ++ "won !")
        Nothing    -> loop newgrid b a


main :: IO () -- Point d'entrée dans le programme
main = do
  -- désactive l'attente de la touche entrée pour l'acquisition
  hSetBuffering stdin NoBuffering
  -- désactive l'écho du caractère entré sur le terminal
  hSetEcho stdin False
  print $ evaluateColumn [Full Red, Empty, Empty, Full Red, Empty, Empty, Full Orange]
  -- lance la REPL
  loop initial ( Human Orange ) ( Computer Red )
