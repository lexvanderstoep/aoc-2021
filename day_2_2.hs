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

type Position = (Int, Int, Int)

applyMove :: Position -> Move -> Position
applyMove (depth, length, aim) (Forward n) = (depth + n * aim, length + n, aim)
applyMove (depth, length, aim) (Up n)      = (depth, length, aim - n)
applyMove (depth, length, aim) (Down n)    = (depth, length, aim + n)

computeAnswer :: String -> Int
computeAnswer input =
    let moves = parseInput input
        (x, y, aim) = foldl applyMove (0, 0, 0) moves
    in x * y

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let answer = computeAnswer input
    putStrLn ("Answer: " ++ show answer)

