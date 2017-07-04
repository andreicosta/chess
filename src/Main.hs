module Main where

import Data.Maybe
import Data.Matrix
import System.Console.ANSI

import Actions
import Init
import Structure

colorPlace :: String
colorPlace = "\x1b[32m"
colorMove :: String
colorMove = "\x1b[31m"
colorAttack :: String
colorAttack = "\x1b[31m"

main :: IO ()
main = do
  setTitle "doubtless chess"
  
  let initBoard = matrix 8 8 starting--Test
      initPlace = (3,5)
  
  loop initBoard Structure.White initPlace
  
  return ()

loop :: Matrix Place -> Player -> Pos -> IO ()
loop oldBoard p place@(x,y) = do
  print ("loop, player " ++ show p ++ " place " ++ show place)
  print "commands: q ENTER w s a d"
  
  let m = postMoveEffects oldBoard
  
  putStrLn (printableMatrix m colorPlace [place] colorMove (getMoves m place) colorAttack (getAttacks m place))
  
  l <- getLine
  
  print ("loop: " ++ l)
  
  let setLeft = if y == 1 then 1 else y-1
      setRight = if y == 8 then 8 else y+1
      setUp = if x == 1 then 1 else x-1
      setDown = if x == 8 then 8 else x+1
  
      (Just getPiece) = piece (getElem x y m)
      notAvailable = isNothing (piece (getElem x y m)) || player getPiece /= p
      
      selectPiece =
        if notAvailable
          then loop m p place
          else selectPieceLoop m p place (place:(getMoves m place ++ getAttacks m place))
  
  case l of
    "q" -> return ()
    "a" -> loop m p (x,setLeft)
    "d" -> loop m p (x,setRight)
    "w" -> loop m p (setUp,y)
    "s" -> loop m p (setDown,y)
    "" -> selectPiece
    _ -> loop m p place
  
  return ()

selectPieceLoop :: Matrix Place -> Player -> Pos -> [Pos] -> IO ()
selectPieceLoop m p place moves = do
  print ("selectPieceLoop, player " ++ show p ++ " place " ++ show place ++ " movements " ++ show moves)
  print "commands: q \ESC ENTER w s"
  
  let actual = head moves
      nextMove = selectPieceLoop m p place (tail moves ++ [actual])
      previousMove = selectPieceLoop m p place (last moves : init moves)
      toMove =
        if actual == place
          then selectPieceLoop m p place moves
          else loop (move m place actual) (changePlayer p) actual
  
  putStrLn (printableMatrix m colorPlace [actual] colorMove (getMoves m place) colorAttack (getAttacks m place))
  
  l <- getLine
  
  print ("selectPieceLoop: " ++ l)
  
  case l of
    "q" -> return ()
    "\ESC" -> loop m p actual
    "" -> toMove
    "s" -> nextMove
    "a" -> nextMove
    "w" -> previousMove
    "d" -> previousMove
    _ -> selectPieceLoop m p place moves
  
  return ()
