module Main where

import Control.Monad (when)

import Data.Maybe
import Data.Matrix
import System.Console.ANSI
import System.Exit

import Actions
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
  
  loop initBoard Structure.White initPlace []

exit :: IO ()
exit = do
  setSGR [Reset]
  exitSuccess

loop :: Board -> Player -> Pos -> [(Pos,Pos)] -> IO ()
loop oldBoard p place@(x,y) history = do
  putStrLn ("loop, player " ++ show p ++ " place " ++ show place)
  putStrLn "commands: q ENTER w s a d back"
  
  let m = postMoveEffects oldBoard
      (Just getPiece) = piece (getElem x y m)
      isTurn = isJust (piece (getElem x y m)) && player getPiece == p
      
      pieceMoves = if isTurn then getMoves m place else []
      pieceAttacks = if isTurn then getAttacks m place else []
     
      notAvailable = isNothing (piece (getElem x y m)) || not isTurn
      
      selectPiece =
        if notAvailable
          then loop m p place history
          else selectPieceLoop m p place (place:(pieceMoves ++ pieceAttacks)) history
      
      backHistory = loop (move m (snd (head history)) (fst (head history))) (changePlayer p) place (tail history)
  
  --putStrLn ("moves " ++ show pieceMoves)
  --putStrLn ("attacks " ++ show pieceAttacks)
  
  when (isCheck m p) (putStrLn "Check!")
  when (isCheckMate m p) (putStrLn "Checkmate!")
  putStrLn (printableMatrix m colorPlace [place] colorMove pieceMoves colorAttack pieceAttacks)
  --when (isCheckMate m p) exit
  
  l <- getLine
  
  putStrLn ("loop: " ++ l)
  
  case l of
    "q" -> exit
    "a" -> loop m p (x,setLeft y) history
    "d" -> loop m p (x,setRight y) history
    "w" -> loop m p (setUp x,y) history
    "s" -> loop m p (setDown x,y) history
    "back" -> if null history then loop m p place history else backHistory
    "" -> selectPiece
    _ -> loop m p place history
  
  return ()

selectPieceLoop :: Board -> Player -> Pos -> [Pos] -> [(Pos,Pos)] -> IO ()
selectPieceLoop m p place moves history = do
  putStrLn ("selectPieceLoop, player " ++ show p ++ " place " ++ show place ++ " movements " ++ show moves)
  putStrLn "commands: q ESC ENTER w s"
  
  let actual@(x,y) = head moves
      moveNear to = selectPieceLoop m p place (nextPos to moves) history
      toMove =
        if actual == place
          then selectPieceLoop m p place moves history
          else loop (move m place actual) (changePlayer p) actual ((place,actual):history)
  
  putStrLn (printableMatrix m colorPlace [actual] colorMove (getMoves m place) colorAttack (getAttacks m place))
  
  l <- getLine
  
  putStrLn ("selectPieceLoop: " ++ l)
  
  case l of
    "q" -> exit
    "\ESC" -> loop m p actual history
    "" -> toMove
    "s" -> moveNear (x+1,y)
    "a" -> moveNear (x,y-1)
    "w" -> moveNear (x-1,y)
    "d" -> moveNear (x,y+1)
    _ -> selectPieceLoop m p place moves history
  
  return ()

setLeft :: Int -> Int
setLeft y = if y == 1 then 1 else y-1

setRight :: Int -> Int
setRight y = if y == 8 then 8 else y+1

setUp :: Int -> Int
setUp x = if x == 1 then 1 else x-1

setDown :: Int -> Int
setDown x = if x == 8 then 8 else x+1
