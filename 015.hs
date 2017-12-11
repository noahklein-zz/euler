main :: IO ()
main = print $ paths (grid 20) (0, 0)

type Grid = (Int, Int)
type Pos = (Int, Int)

grid :: Int -> Grid
grid x = (x, x)

paths :: Grid -> Pos -> Int
paths grid@(width, height) pos@(x, y)
  | x == width && y == height = 1
  | otherwise                 = paths' pos
  where paths' = sum . (fmap (paths grid)) . (neighbors grid)

neighbors :: Grid -> Pos -> [Pos]
neighbors (width, height) (x, y) = mappend xNeighbor yNeighbor
  where
    xNeighbor = [(x + 1, y) | x < width]
    yNeighbor = [(x, y + 1) | y < height]
