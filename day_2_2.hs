import Data.List (unfoldr)

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

data Move =
    Forward Int |
    Down Int |
    Up Int

parseMove :: String -> Move
parseMove inputMove =
    let direction = (separateBy ' ' inputMove) !! 0
        size = (separateBy ' ' inputMove) !! 1
    in
        if direction == "forward"
            then Forward (read size)
        else if direction == "up"
            then Up (read size)
        else
            Down (read size)

parseInput :: String -> [Move]
parseInput input = map parseMove (lines input)

type Position = (Int, Int, Int)

applyMove :: Position -> Move -> Position
applyMove (depth, length, aim) (Forward n) = (depth + n * aim, length + n, aim)
applyMove (depth, length, aim) (Up n)      = (depth, length, aim - n)
applyMove (depth, length, aim) (Down n)    = (depth, length, aim + n)

applyMoves :: [Move] -> Position -> Position
applyMoves moves position = foldl applyMove position moves

computeAnswer :: String -> Int
computeAnswer input =
    let moves = parseInput input
        (x, y, aim) = applyMoves moves (0, 0, 0)
    in x * y

main :: IO ()
main = do
   input <- readFile "input_2_2.txt"
   let answer = computeAnswer input
   putStrLn ("Answer: " ++ show answer)

