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
                10 ^ num * case c of
                  Full Red -> 1
                  Full Orange -> -1
                  Empty -> 0
            )).summarizeGrid

negaMax::Color->Int->Int->Grid -> Int
negaMax color d dmax grid | dmax == d = (if color==Red then 1 else -1) * (evaluate grid)

negaMax color d dmax grid =
  let nextCol = otherColor color
      playit  = flip (play color) grid
  in
    maximum (map (((-1)*).(negaMax nextCol (d+1) dmax).playit) (legalMoves grid))

aimove::Color->Grid->Int
aimove color grid = fst $ maximumBy (compare `on` snd)
                            (map
                              (\m -> (m, -(negaMax (otherColor color) 0 1 (play color m grid))))
                              (legalMoves grid))

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
  -- lance la REPL
  loop initial ( Human Red ) ( Computer Orange )
