import Data.Maybe  (isNothing,mapMaybe)
import Data.Matrix (Matrix,matrix,getElem,setElem)

import Actions
import Init
import Structure

main = do
  print board9
  return 0

board = matrix 8 8 starting
board2 = walking board (2,5)
board3 = walking board2 (1,2)
board4 = walking board3 (2,2)
board5 = walking board4 (4,2)
board6 = walking board5 (1,1)
board7 = walking board6 (1,3)
board8 = walking board7 (1,5)
board9 = walking board8 (3,5)

walking :: Matrix Place -> (Int,Int) -> Matrix Place
walking m pos@(i,j) = move m pos (last movs)
  where
    elem = getElem i j m
    (Just whatIsThere) = piece elem
    movs = allMoves m pos
