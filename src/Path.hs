module Path
  ( nextPos
  ) where

import Data.Function (on)
import Data.List     (sortBy)

import Structure     (Pos)

-- | It receives a list of possible movements `l` where `head l` is the
-- actual piece position, it sort the next positions by the nearest of
-- `pos` which is the desired final position.
nextPos :: Pos -> [Pos] -> [Pos]
nextPos pos l = sortNearPos pos (tail l) ++ [head l]

sortNearPos :: Pos -> [Pos] -> [Pos]
sortNearPos pos = sortBy (compare `on` dist pos)

dist :: Pos -> Pos -> Double
dist (x1,y1) (x2,y2) = sqrt (fromIntegral ((x2-x1)*(x2-x1)) + fromIntegral ((y2-y1)*(y2-y1)))
