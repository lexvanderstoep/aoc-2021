import Data.List (unfoldr)
import Data.List.Split (splitOn)
import System.Environment

data Move =
    Forward Int |
    Down Int |
    Up Int

parseMove :: String -> Move
parseMove inputMove =
    let splitInputMove = splitOn " " inputMove
    in
        if length splitInputMove < 2
            then error ("Invalid move: " ++ inputMove)
        else
            let direction = splitInputMove !! 0
                length = read (splitInputMove !! 1)
    in
        if direction == "forward"
            then Forward length
        else if direction == "up"
            then Up length
        else if direction == "down"
            then Down length
        else error ("Unrecognised direction: " ++ direction)


parseInput :: String -> [Move]
parseInput input = map parseMove (lines input)

type Position = (Int, Int)

applyMove :: Position -> Move -> Position
applyMove (depth, length) (Forward n) = (depth, length + n)
applyMove (depth, length) (Up n)      = (depth - n, length)
applyMove (depth, length) (Down n)    = (depth + n, length)

computeAnswer :: String -> Int
computeAnswer input =
    let moves = parseInput input
        (x, y) = foldl applyMove (0, 0) moves
    in x * y

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let answer = computeAnswer input
    putStrLn ("Answer: " ++ show answer)

