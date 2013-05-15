import Prelude hiding (Either (..))
import Data.Char (ord, chr)
import System.Environment (getArgs)
import Data.Vector (Vector, (!), fromList)

import Tape

data Symbol = Star | Plus | NOP deriving (Eq, Show)
data Direction = Up | Down | Left | Right deriving (Eq, Show)
type Coordinate = (Int, Int)
type Program = Vector (Vector Symbol)
newtype State = State (Tape Int, Program, Coordinate, Direction) deriving (Show)

execute :: String -> IO ()
execute input = do
    let tape = (moveTapeRight . moveTapeRight) $ makeTape [0,maxBound::Int,0] 0
        program = parse input
        state = State (tape, program, (0,0), Down)
    (State (t, _, c, d)) <- run state
    putStrLn $ "Ended execution at " ++ show c ++ " moving " ++ show d
    putStrLn $ "Final tape: " ++ show t

parse :: String -> Program
parse input = fromList $ map vectorize rows
    where rows = lines input
          vectorize line = fromList $ map tokenize line ++ replicate (maxLen - (length line)) NOP
          maxLen = maximum (map length rows)
          tokenize '*' = Star
          tokenize '+' = Plus
          tokenize _   = NOP

run :: State -> IO (State)
run s@(State (_, _, (-1, _), _)) = return s    -- left coordinate out of bounds, stop execution
run s@(State (_, _, (_, -1), _)) = return s    -- top coordinate out of bounds, stop execution
run (State (tape, prog, pos, dir)) =
    (putStrLn $ "Tape: " ++ show tape ++ " Pos: " ++ show pos ++ " Dir: " ++ show dir) >> 
    case symbolAt prog pos of
        NOP  -> run (State (tape, prog, next, dir))
        Star -> if (isTL1 tape && (dir == Left || dir == Right)) then   -- I/O action
                    let tl0 = readTape (moveTapeLeft tape) in
                    if (tl0 == 0) then getChar >>= (\i -> run (State (moveTapeRight (writeTape (moveTapeLeft tape) (ord i)), prog, next, dir)))
                    else putChar (chr tl0) >> run (State (tape, prog, next, dir))
                else                                                    -- normal star
                    case dir of
                        Up    -> run (State (moveTapeRight tape, prog, next, dir))
                        Down  -> run (State (moveTapeLeft tape, prog, next, dir))
                        Left  -> run (State (updateTape tape (subtract 1), prog, next, dir))
                        Right -> run (State (updateTape tape (+1), prog, next, dir))
        Plus -> if (readTape tape == 0) then let (nc, nd) = leftTurn pos dir in run (State (tape, prog, nc, nd))
                else let (nc, nd) = rightTurn pos dir in run (State (tape, prog, nc, nd))
    where next = nextCoord pos dir

isTL1 :: (Eq a, Bounded a) => Tape a -> Bool
isTL1 t = readTape t == maxBound

symbolAt :: Program -> Coordinate -> Symbol
symbolAt p (x, y) = p ! y ! x

nextCoord :: Coordinate -> Direction -> Coordinate
nextCoord (x, y) d  | d == Up       = (x,y-1)
                    | d == Down     = (x,y+1)
                    | d == Left     = (x-1,y)
                    | d == Right    = (x+1,y)

leftTurn :: Coordinate -> Direction -> (Coordinate, Direction)
leftTurn (x, y) d   | d == Up       = ((x-1, y+1), Left)
                    | d == Down     = ((x+1, y-1), Right)
                    | d == Left     = ((x+1, y+1), Down)
                    | d == Right    = ((x-1, y-1), Up)

rightTurn :: Coordinate -> Direction -> (Coordinate, Direction)
rightTurn (x, y) d  | d == Up       = ((x+1, y+1), Right)
                    | d == Down     = ((x-1, y-1), Left)
                    | d == Left     = ((x+1, y-1), Up)
                    | d == Right    = ((x-1, y+1), Down)

main :: IO ()
main = do
    args <- getArgs
    if (length args /= 1) then
        putStrLn "Usage: 2L <file>"
    else do
        input <- readFile (head args)
        execute input
