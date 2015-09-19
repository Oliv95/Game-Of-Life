
--------------------------------------------------------------------------------
--  imports

import qualified Data.Foldable as Fld
import qualified Data.Sequence as Sec
import Data.List.Split
import System.Posix.Unistd
import System.Console.ANSI
import System.Environment

--------------------------------------------------------------------------------
--  data type declarations

data State = Alive | Dead
    deriving (Eq,Show)

data Board = Board [[State]] Pos
    deriving (Eq,Show)

--           row   col
--           heigt width
type Pos   = (Int,Int)

--------------------------------------------------------------------------------
--  testing functions

emptyBoard :: Board
emptyBoard = Board (replicate 9 (replicate 8 Dead)) (8,7)

fullBoard :: Board
fullBoard = Board (replicate 9 (replicate 8 Alive)) (8,7)

lagomBoard = unlines $ ["     ", "  xx ", "   xx", "xxxx ", "  x x"]

getBoard :: Board -> [[State]]
getBoard (Board b _) = b

--------------------------------------------------------------------------------
--  generic helper functions

count :: (Eq a,Num b) => [a] -> a -> b
count xs e = count' xs e 0
    where count' []  _ _    = 0
          count' [x] e n    = if e==x then n+1 else n
          count' (x:xs) e n = if e==x then count' xs e n+1 else count' xs e n

--------------------------------------------------------------------------------
-- impure IO functions
main :: IO()
main = do
    args      <- getArgs
    content   <- readFile $ head $ args
    let board = readBoard content
    startCycle board
    return ()

halfsecond = 500000000

startCycle b@(Board _ (_,h)) = do
    clearScreen
    cursorUpLine (h+1)
    putStrLn $ showBoard b
    nanosleep halfsecond
    startCycle $ nextCycle b

--------------------------------------------------------------------------------
--  reading / showing gameboard

readState :: Char -> State
readState ' ' = Dead
readState 'x' = Alive
readState _   = error "Parse error in readState"

readStateList :: String -> [State]
readStateList l = map readState l

showSquare :: State -> String
showSquare Dead  = " "
showSquare Alive = "x"

showSquareList :: [State] -> String
showSquareList l = concat (map showSquare l)

showBoard ::  Board -> String
showBoard (Board b _) = unlines $ map showSquareList b

readBoard :: String -> Board
readBoard s = Board (map readStateList l) (h,w)
        where l = lines s
              h = (length l) - 1
              w = (length $ head l) - 1

--------------------------------------------------------------------------------
--  state functions

isDead :: State -> Bool
isDead Dead  = True
isDead Alive = False

isAlive :: State -> Bool
isAlive s = not $ isDead s

--------------------------------------------------------------------------------
--  gameboard functions

inRange :: Board -> Pos -> Bool
inRange (Board _ (h,w)) (x,y) = and [x>=0,x<=w,y>=0,y<=h]

stateAt :: Board -> Pos -> State
stateAt (Board l _) (row,col) = (l !! col) !! row

updateState ::  Board -> Pos -> State -> Board
updateState (Board b p) (x',y') s = Board (above ++ [row] ++ below) p
    where above = take x' b
          row   = Fld.toList $ Sec.update y' s $ (Sec.fromList (b !! x'))
          below = drop (x'+1) b

neighbours ::  Board -> Pos -> [State]
neighbours b (x,y) = map (stateAt b) $ filter (inRange b) $ [(x',y') | 
                                                     x' <- [x-1 .. x+1],
                                                     y' <- [y-1 .. y+1],
                                                     (x',y') /= (x,y)]

liveNeighbours :: (Num b) => Board -> Pos -> b
liveNeighbours b p = count (neighbours b p) Alive 

{-- Any live cell with exactly 2 or 3 live neighbours will
    survive to the next cycle
--}

live :: Board -> Pos -> Bool
live b p 
    | isDead $ stateAt b p = error "Dead state applied to live"
    | liveNeighbours b p `elem` [2,3]   = True
    | otherwise                         = False

{-- Any dead cell with exactly 3 live neightbours will be born
    the next cycl
--}
born :: Board -> Pos -> Bool
born b p 
    | isAlive $ stateAt b p = error "Alive state applied to born"
    | liveNeighbours b p == 3 = True
    | otherwise               = False

--------------------------------------------------------------------------------
--  cycle functions

nextGen :: Board -> Pos -> State
nextGen b p 
    | isDead $ stateAt b p = if born b p then Alive else Dead
    | otherwise            = if live b p then Alive else Dead

allPos :: Int -> Int -> [Pos]
allPos w h = [(h',w') | w' <- [0..w], h' <- [0..h]]

nextCycle :: Board -> Board
nextCycle board@(Board b (h,w)) = (Board states (h,w))
    where states = chunksOf (w+1) [nextGen board p | p <- allPos h w]
