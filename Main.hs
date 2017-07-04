import Data.Maybe
import Data.Matrix
import System.Console.ANSI
import System.IO

import Actions
import Init
import Structure

main = do
  setTitle "doubtless chess"
  
  let initBoard = matrix 8 8 starting
      initPlace = (7,8)
  
  loop initBoard initPlace
  
  return ()

loop m place@(x,y) = do
  print ("loop: q ENTER w s a d")
  
  putStrLn (printableMatrix m "\x1b[32m" [place] "\x1b[31m" (getMoves m place))
  
  l <- getLine
  
  print ("loop: " ++ l)
  
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
    "" -> selectPieceLoop m place (place:(getMoves m place))
    _ -> loop m place
  
  return ()

selectPieceLoop m place moves = do
  print ("selectPieceLoop: q \ESC ENTER w s")
  
  let actual@(x,y) = head moves
  
  putStrLn (printableMatrix m "\x1b[32m" [actual] "\x1b[31m" (getMoves m place))
  
  l <- getLine
  
  print ("selectPieceLoop: " ++ l)
  
  case l of
    "q" -> return ()
    "\ESC" -> loop m actual
    "" -> if actual == place then selectPieceLoop m place moves else loop (move m place actual) actual
    "s" -> selectPieceLoop m place (tail moves ++ [actual])
    "w" -> selectPieceLoop m place (last moves : init moves)
    _ -> selectPieceLoop m place moves
  
  return ()
