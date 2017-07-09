module Main where

import Data.Matrix
import qualified System.Console.ANSI as ANSI
import System.Exit

import Init
import Player.Thinker
import Simulation
import Structure

colorPlace :: String
colorPlace = "\x1b[32m"
colorMove :: String
colorMove = "\x1b[31m"
colorAttack :: String
colorAttack = "\x1b[31m"

main :: IO ()
main = do
  ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Blue]
  ANSI.setTitle "Helping Human: doubtless chess"

  let initBoard = matrix 8 8 starting
      moves = [Movement (7,5) (5,5) (Piece Pawn White) [PawnDoubleMove]
              ,Movement (2,5) (4,5) (Piece Pawn Black) [PawnDoubleMove]
              ,Movement (7,4) (6,4) (Piece Pawn White) []
              ,Movement (1,7) (3,6) (Piece Knight Black) []
              ,Movement (8,3) (4,7) (Piece Bishop White) []
              ,Movement (2,8) (3,8) (Piece Pawn Black) []
              ,Movement (4,7) (6,5) (Piece Bishop White) []
              ,Movement (3,6) (5,7) (Piece Knight Black) []
              ,Movement (8,4) (5,7) (Piece Queen White) [Attack (Piece Knight Black)]
              ,Movement (2,4) (3,4) (Piece Pawn Black) []
              ,Movement (5,7) (4,8) (Piece Queen White) []
              ,Movement (2,3) (3,3) (Piece Pawn Black) []
              ,Movement (7,7) (6,7) (Piece Pawn White) []
              ,Movement (1,4) (3,2) (Piece Queen Black) []
              ,Movement (6,5) (3,2) (Piece Bishop White) [Attack (Piece Queen Black)]
              ,Movement (2,1) (3,2) (Piece Pawn Black) [Attack (Piece Bishop White)]
              ,Movement (8,6) (6,8) (Piece Bishop White) []
              ,Movement (1,3) (6,8) (Piece Bishop Black) [Attack (Piece Bishop White)]
              ,Movement (8,7) (6,8) (Piece Knight White) [Attack (Piece Bishop Black)]
              ,Movement (2,7) (3,7) (Piece Pawn Black) []
              ,Movement (4,8) (6,6) (Piece Queen White) []
              ,Movement (1,6) (2,7) (Piece Bishop Black) []
              ,Movement (6,6) (6,5) (Piece Queen White) []
              ,Movement (3,2) (4,2) (Piece Pawn Black) []
              ,Movement (6,5) (3,2) (Piece Queen White) []
              ,Movement (1,2) (2,4) (Piece Knight Black) []
              ,Movement (3,2) (2,2) (Piece Queen White) [Attack (Piece Pawn Black)]
              ,Movement (1,1) (1,4) (Piece Rook Black) []
              ,Movement (2,2) (3,3) (Piece Queen White) [Attack (Piece Pawn Black)]
              ,Movement (1,5) (1,7) (Piece King Black) [Castling]
              --,Movement (3,3) (3,4) (Piece Queen White) [Attack (Piece Pawn Black)]
              --,Movement (2,4) (1,2) (Piece Knight Black) []
              --,Movement (3,4) (4,5) (Piece Queen White) [Attack (Piece Pawn Black)]
              --,Movement (8,5) (8,7) (Piece King White) [Castling]
              --,Movement (2,7) (7,2) (Piece Bishop Black) [Attack (Piece Pawn White)]
              --,Movement (8,2) (7,4) (Piece Knight White) []
              --,Movement (7,2) (1,1) (Piece Bishop Black) [Attack (Piece Rook White)]
              ]
      applyMoves = applyMovementsTest initBoard [] moves

  loop applyMoves White []

loop :: Board -> Player -> History -> IO ()
loop m p history = do
  putStrLn (printableMatrix m colorPlace [] colorMove [] colorAttack [])

  let playerMovements = Player.Thinker.whichMoves m p history

  putStrLn ((concatMap (\m -> show m ++ "\n")) playerMovements)

  ANSI.setSGR [ANSI.Reset]
  putStrLn "\n"
  exitSuccess
