import Data.Maybe (listToMaybe, catMaybes)


-- types declarations
type Point = (Int, Int)
type Maze = [String]
type Path = [Point]
type Seen = [Point]
-- maze creation
maze :: Maze

maze = [
    "###   #E##",
    "##  #   ##",
    "##S#######"
  ]

seen :: Seen
seen = []

-- symbol creations
escapeSymbol :: Char
escapeSymbol = 'E'

wallSymbol :: Char
wallSymbol = '#'

-- directions abstraction
directions :: [Point]
directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

isOutOfBounds :: Point -> Maze -> Bool
--isOutOfBounds (x, y) maze = x < 0 || y < 0 || y <= length maze || x >= length (maze !! y)

--isOutOfBounds point maze = 
--let (x, y) = point
--in x < 0 || y < 0 || y >= length maze || x >= length (maze !! y)

isOutOfBounds (x, y) maze  
 | y < 0 || y >= length maze = True
 | otherwise = x < 0 || x >= length (maze !! y)

isWall :: Point -> Maze -> Bool
isWall (x, y) maze = (maze !! y) !! x == wallSymbol

isEscape :: Point -> Maze -> Bool
isEscape (x, y) maze = (maze !! y) !! x == escapeSymbol

getX :: Point -> Int
getY :: Point -> Int
getX = fst -- first
getY = snd -- second 

findPath :: Maze -> Point -> Seen -> Path -> Maybe [Point]
findPath maze (x, y) seen path
 | (x, y) `elem` seen = Nothing 
 | isOutOfBounds (x, y) maze = Nothing
 | isWall (x, y) maze = Nothing
 | isEscape (x, y) maze = Just ((x, y) : path)
 | otherwise = listToMaybe . catMaybes $ map continuePath directions
 where 
  seen' = (x, y) : seen
  continuePath direction = findPath maze (x + fst direction, y + snd direction) seen' ((x, y) : path)

printPath :: Maybe [Point] -> IO ()
printPath Nothing = putStrLn "No path found"
printPath (Just path) = mapM_ print path

main :: IO ()

main = do
 let start = (2, 2)
 let resultpath = findPath maze start seen []
 printPath resultpath









