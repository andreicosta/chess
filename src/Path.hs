module Path
  ( nextPos
  ) where

import Data.Function (on)
import Data.List     (sortBy)

import Structure     (Movement,Pos,target)

-- | It receives a list of possible movements `l` where `head l` is the
-- actual piece position, it sort the next positions by the nearest of
-- `pos` which is the desired final position.
nextPos :: Pos -> [Movement] -> [Movement]
nextPos pos l = sortNearPos pos (tail l) ++ [head l]

sortNearPos :: Pos -> [Movement] -> [Movement]
sortNearPos pos = sortBy (compare `on` dist pos)

dist :: Pos -> Movement -> Double
dist (x1,y1) move = sqrt (fromIntegral ((x2-x1)*(x2-x1)) + fromIntegral ((y2-y1)*(y2-y1)))
  where
    (x2,y2) = target move
