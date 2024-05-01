import Data.Graph (path)
import Data.Maybe (listToMaybe, catMaybes)

type Point = (Int, Int)
type Maze = [String]

maze :: Maze

maze = [
    "###   #E##",
    "##  #   ##",
    "##S#######"
  ]

escapeSymbol :: Char
escapeSymbol = 'E'

wallSymbol :: Char
wallSymbol = '#'

directions :: [Point]
directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]


isOutOfBounds :: Point -> Maze -> Bool
isOutOfBounds (x, y) maze = x < 0 || y < 0 || y <= length maze || x >= length (maze !! y)

isWall :: Point -> Maze -> Bool
isWall (x, y) maze = (maze !! y) !! x == escapeSymbol


findPath :: Maze -> Point -> [Point] -> Maybe [Point]
findPath maze (x, y) path
 | isOutOfBounds (x, y) maze = Nothing
 | isWall (x, y) maze = Nothing
 | (x, y) `elem` path = Nothing
 | isEscape (x, y) maze = Just ((x, y) : path)
 | otherwise = listToMaybe . catMaybes $ map (\dir -> findPath maze (x + fst dir, y + snd dir) ((x, y) : path)) directions


printPath :: Maybe [Point] -> IO ()
printPath Nothing = putStrLn "No path found"
printPath (Just path) = mapM_ print path

main :: IO ()

main = do
 let start = (2 , 2)
 let resultpath = findPath maze start []
 printPath resultpath









