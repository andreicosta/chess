module Main where

import Control.Monad (unless,when)

import Data.Matrix
import System.Console.ANSI
import System.Exit
import System.Random

import Actions
import Init
import Player
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
  setTitle "Computer vs Computer: doubtless chess"
  
  let initBoard = matrix 8 8 starting
  
  loop initBoard Structure.White []

exit :: IO ()
exit = do
  setSGR [Reset]
  exitSuccess

loop :: Board -> Player -> History -> IO ()
loop m p history = do
  putStrLn ("loop, player " ++ show p)
  putStrLn "commands: q ENTER back"
  
  unless
    (null (listPawnPromotion m))
    (loop (pawnPromotion m Queen (head (listPawnPromotion m))) p history)
  
  when (isCheck m p history) (putStrLn "Check!")
  when (isCheckMate m p history) (putStrLn "Checkmate!")
  putStrLn (printableMatrix m colorPlace [] colorMove [] colorAttack [])
  --when (isCheckMate m p history) exit

  g <- newStdGen
  
  let backHistory = loop (undoMovement m (head history)) (changePlayer p) (tail history)
      playerMovement = whichMove m p history g
      moveOrAttack = if isAttack playerMovement then attack m playerMovement else move m playerMovement
  
  print playerMovement

  l <- getLine

  putStrLn ("loop: " ++ l)

  case l of
    "q" -> exit
    "back" -> if null history then loop m p history else backHistory
    "" -> loop moveOrAttack (changePlayer p) (addHistory history playerMovement)
    _ -> loop m p history
  
  return ()
