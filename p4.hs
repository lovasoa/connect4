import Data.List
import Data.Maybe
import Data.Function
import Data.Char
import System.IO

import Debug.Trace

wonAt = 4
maxDepth = 7

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

-- Get the lines, columns and diagonals of a grid
allAlignments::Grid->[Column]
allAlignments grid = concatMap ($ grid)
                      [getDiagonals, getDiagonals.(map reverse), id, transpose]

getDiagonals:: Grid->[Column]
getDiagonals grid = takeWhile (not.null) $ map catMaybes $ transpose $
                      map
                        (\(l,n)->
                          (replicate n Nothing)++(map (Just) l)++(repeat Nothing))
                        (zip grid [0..])

-- Returns the color of the winner, or Nothing if there is none
won:: Grid -> Maybe Color
won grid = let colorWon = or.sequence (map wonCol (allAlignments grid)) in
            listToMaybe $ filter (colorWon) [Red, Orange]

wonCol:: Column -> Color -> Bool
wonCol [] _ = False
wonCol column color = ((>=wonAt) $ length $ takeWhile (==Full color) column)
                        || (wonCol (tail column) color)


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

-- Negamax, with alpha-beta pruning
-- Returns a tuple.
--  - The first element represents a move (0-6)
--  - The second represents how advantageous that move is for the given color
--  depth represents the number of moves to foresee
negaMaxAB::Color->Int->(Int, Int)->Grid -> (Int, Int)
negaMaxAB color depth _ grid | depth == 0 || (isJust $ won grid) =
  let evaluation = (if color==Red then 1 else -1) * (evaluate grid) in
    (0, evaluation)
negaMaxAB color depth (a,b) grid =
  let nextCol = otherColor color
      playit  = flip (play color) grid
  in
    snd$foldr
        (\m ((a,b),(bmove,beval)) -> 
            if a>=b then ((a,b),(bmove,beval)) else
            let neg  = -(snd$negaMaxAB nextCol (depth-1) (-b,-a) (playit m))
                newa = max a neg
            in
            ( (newa,b),
              if neg>beval then (m,neg) else (bmove,beval)
            )
        )
        ((a,b), (0,a))
        (legalMoves grid)

aimove::Color->Grid->Int
aimove color grid = fst $ negaMaxAB color maxDepth (-10^5,10^5) grid

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
  -- lance la REPL
  loop initial ( Computer Orange ) ( Computer Red )
