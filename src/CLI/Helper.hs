module Main where

import Data.Matrix
import qualified System.Console.ANSI as ANSI
import System.Exit

import Init
--import Player.Thinker
import Player.Thinker2
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
  ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Black]
  ANSI.setTitle "Helping Human: doubtless chess"

  c <- readFile "moves.txt"

  let initBoard = matrix 8 8 starting
      moves = read c :: [Movement]
      board = applyMovementsTest initBoard [] moves

  putStrLn (printableMatrix board colorPlace [] colorMove [] colorAttack [])

  let p = Black
      history = reverse moves
      --playerMovements = Player.Thinker.whichMoves board p history
      playerMovements = Player.Thinker2.whichMoves board p history

  putStrLn (concatMap (\m -> show m ++ "\n") playerMovements)

  ANSI.setSGR [ANSI.Reset]
  putStrLn "\n"
  exitSuccess
