import Data.Maybe
import Data.Matrix
import System.Console.ANSI
import System.IO

import Actions
import Init
import Structure

main = do
  setTitle "doubtless chess"
  
  let initPlace = (7,8)
      --playablePieces = filter (\p -> player p == Structure.White) (mapMaybe piece (toList board))
      movements = matrix 8 8 (getMoves board)
  --print (getMoves board initPlace)
  --putStrLn (printableMatrix board "\x1b[32m" [initPlace] (getMoves board initPlace))
  
  loop board initPlace
  
  return 0

loop m place@(x,y) = do
  putStrLn (printableMatrix board "\x1b[32m" [place] (getMoves m place))
  
  l <- getLine
  
  print l
  
  let setLeft = if y == 1 then 1 else y-1
  let setRight = if y == 8 then 8 else y+1
  let setUp = if x == 1 then 1 else x-1
  let setDown = if x == 8 then 8 else x+1
  
  case l of
    "q" -> return ()
    "a" -> loop m (x,setLeft)
    "d" -> loop m (x,setRight)
    "w" -> loop m (setUp,y)
    "s" -> loop m (setDown,y)
    _ -> loop m place
  
  return ()
  
getMoves m (x,y) = if isNothing (piece elem) then [] else allMoves m (x,y)
  where
    elem = getElem x y m
    (Just whatIsThere) = piece elem
    

mainOld = do
  setTitle "doubtless chess"
  
  getChar
  putStrLn $ "\x1b[32m"
  clearScreen
  print board
  
  getChar
  clearScreen
  print board2
  putStrLn $ "\x1b[37m"
  
  getChar
  clearScreen
  print board3
  
  getChar
  clearScreen
  print board4
  
  getChar
  clearScreen
  print board5
  
  getChar
  clearScreen
  print board6
  
  getChar
  clearScreen
  print board7
  
  getChar
  clearScreen
  print board8
  
  getChar
  clearScreen
  print board9
  
  setSGR [Reset]
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
