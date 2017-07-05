module Main where

import Control.Monad (when)

import Data.Maybe
import Data.Matrix
import System.Console.ANSI
import System.Exit

import Actions
import Check
import Init
import Path
import Structure
import Util

colorPlace :: String
colorPlace = "\x1b[32m"
colorMove :: String
colorMove = "\x1b[31m"
colorAttack :: String
colorAttack = "\x1b[31m"

main :: IO ()
main = do
  setSGR [SetColor Background Dull System.Console.ANSI.Black]
  setTitle "doubtless chess"
  
  let initBoard = matrix 8 8 starting--Test2
      initPlace = (7,5)
  
  loop initBoard Structure.White initPlace

exit :: IO ()
exit = do
  setSGR [Reset]
  exitSuccess

loop :: Matrix Place -> Player -> Pos -> IO ()
loop oldBoard p place@(x,y) = do
  print ("loop, player " ++ show p ++ " place " ++ show place)
  print "commands: q ENTER w s a d"
  
  let m = postMoveEffects oldBoard
  
  when (isCheck m p) (putStrLn "Check!")
  when (isCheckMate m p) (putStrLn "Checkmate!\nExiting...")
  putStrLn (printableMatrix m colorPlace [place] colorMove (getMoves m place) colorAttack (getAttacks m place))
  when (isCheckMate m p) exit
  
  l <- getLine
  
  print ("loop: " ++ l)
  
  let (Just getPiece) = piece (getElem x y m)
      notAvailable = isNothing (piece (getElem x y m)) || player getPiece /= p
      
      selectPiece =
        if notAvailable
          then loop m p place
          else selectPieceLoop m p place (place:(getMoves m place ++ getAttacks m place))
  
  case l of
    "q" -> exit
    "a" -> loop m p (x,setLeft y)
    "d" -> loop m p (x,setRight y)
    "w" -> loop m p (setUp x,y)
    "s" -> loop m p (setDown x,y)
    "" -> selectPiece
    _ -> loop m p place
  
  return ()

selectPieceLoop :: Matrix Place -> Player -> Pos -> [Pos] -> IO ()
selectPieceLoop m p place moves = do
  print ("selectPieceLoop, player " ++ show p ++ " place " ++ show place ++ " movements " ++ show moves)
  print "commands: q \ESC ENTER w s"
  
  let actual@(x,y) = head moves
      moveNear to = selectPieceLoop m p place (nextPos to moves)
      toMove =
        if actual == place
          then selectPieceLoop m p place moves
          else loop (move m place actual) (changePlayer p) actual
  
  putStrLn (printableMatrix m colorPlace [actual] colorMove (getMoves m place) colorAttack (getAttacks m place))
  
  l <- getLine
  
  print ("selectPieceLoop: " ++ l)
  
  case l of
    "q" -> exit
    "\ESC" -> loop m p actual
    "" -> toMove
    "s" -> moveNear (x+1,y)
    "a" -> moveNear (x,y-1)
    "w" -> moveNear (x-1,y)
    "d" -> moveNear (x,y+1)
    _ -> selectPieceLoop m p place moves
  
  return ()

setLeft :: Int -> Int
setLeft y = if y == 1 then 1 else y-1

setRight :: Int -> Int
setRight y = if y == 8 then 8 else y+1

setUp :: Int -> Int
setUp x = if x == 1 then 1 else x-1

setDown :: Int -> Int
setDown x = if x == 8 then 8 else x+1
