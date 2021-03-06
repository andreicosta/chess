module Main where

import Control.Monad (unless,when)

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

loop :: Board -> Player -> Pos -> History -> IO ()
loop m p place@(x,y) history = do
  putStrLn ("loop, player " ++ show p ++ " place " ++ show place)
  putStrLn "commands: q ENTER w s a d back"
  
  unless
    (null (listPawnPromotion m))
    (updatePawnLoop m p (head (listPawnPromotion m)) place history)
  
  let (Just getPiece) = piece (getElem x y m)
      isTurn = isJust (piece (getElem x y m)) && player getPiece == p

      movementsMoves = getMoves m place history
      movementsAttacks = getAttacks m place history
      allMovements = movementsMoves ++ movementsAttacks

      pieceMoves = if isTurn then map target movementsMoves else []
      pieceAttacks = if isTurn then map target movementsAttacks else []

      notAvailable = isNothing (piece (getElem x y m)) || not isTurn

      fakeMove = Movement place place undefined []
      orderedMovements = fakeMove : allMovements
      selectPiece =
        if notAvailable
          then loop m p place history
          else selectPieceLoop m p place orderedMovements history

      backHistory = loop (undoMovement m (head history)) (changePlayer p) place (tail history)

  putStrLn ("history " ++ show history)
  putStrLn ("moves " ++ show pieceMoves)
  putStrLn ("attacks " ++ show pieceAttacks)

  when (isCheck m p history) (putStrLn "Check!")
  when (isCheckMate m p history) (putStrLn "Checkmate!")
  putStrLn (printableMatrix m colorPlace [place] colorMove pieceMoves colorAttack pieceAttacks)
  --when (isCheckMate m p history) exit

  l <- getLine

  putStrLn ("loop: " ++ l)

  case l of
    "q" -> exit
    "a" -> loop m p (x,setLeft y) history
    "d" -> loop m p (x,setRight y) history
    "w" -> loop m p (setUp x,y) history
    "s" -> loop m p (setDown x,y) history
    "back" -> if null history then loop m p place history else backHistory
    "" -> if null allMovements then loop m p place history else selectPiece
    _ -> loop m p place history
  
  return ()

updatePawnLoop :: Board -> Player -> (Rank, File) -> Pos -> History -> IO ()
updatePawnLoop board p pos place history = do
  putStrLn ("updatePawnLoop, replace Pawn on " ++ show pos)
  putStrLn "commands: Queen Rook Bishop Knight"
  
  l <- getLine

  putStrLn ("updatePawnLoop: " ++ l)

  case l of
        "Queen" -> loop (pawnPromotion board Queen pos) p place history
        "Rook" -> loop (pawnPromotion board Rook pos) p place history
        "Bishop" -> loop (pawnPromotion board Bishop pos) p place history
        "Knight" -> loop (pawnPromotion board Knight pos) p place history
        _ -> updatePawnLoop board p pos place history

selectPieceLoop :: Board -> Player -> Pos -> [Movement] -> History -> IO ()
selectPieceLoop m p piecePlace moves history = do
  putStrLn ("selectPieceLoop, player " ++ show p ++ " place " ++ show piecePlace ++ " movements " ++ concatMap show_ moves)
  putStrLn "commands: q ESC ENTER w s"
  
  let actual = head moves
      (x,y) = target actual
      
      pieceMoves = map target (getMoves m piecePlace history)
      pieceAttacks = map target (getAttacks m piecePlace history)
      
      moveNear to = selectPieceLoop m p piecePlace (nextPos to moves) history
      moveOrAttack = if isAttack actual then attack m actual else move m actual
      toMove =
        if (x,y) == piecePlace
          then selectPieceLoop m p piecePlace moves history
          else loop moveOrAttack (changePlayer p) (x,y) (addHistory history actual)
  
  putStrLn (printableMatrix m colorPlace [(x,y)] colorMove pieceMoves colorAttack pieceAttacks)
  
  l <- getLine
  
  putStrLn ("selectPieceLoop: " ++ l)
  
  case l of
    "q" -> exit
    "\ESC" -> loop m p (target actual) history
    "" -> toMove
    "s" -> moveNear (x+1,y)
    "a" -> moveNear (x,y-1)
    "w" -> moveNear (x-1,y)
    "d" -> moveNear (x,y+1)
    _ -> selectPieceLoop m p piecePlace moves history
  
  return ()
