module Player
  ( whichMove
  ) where

import Data.Maybe
import Data.Matrix
import System.Random

import Actions
import Init
import Structure
import Util

moveOrAttack m mv = if isAttack mv then attack m mv else move m mv

--whichMove :: Board -> Player -> History -> Movement
whichMove b p h g = if null choosenList then err else choosenList !! (rand choosenList)
  where
    err = error ("player " ++ show p ++ " no moves")
    rand list = fst (randomR (0::Int, (length list - 1)::Int) g)
    choosenList
      | not (null winGame) = winGame
      | not (null giveCheck) = giveCheck
      | not (null toAttack) = toAttack
      | otherwise = allMovements
    
    winGame = filter (\mv -> isCheckMate b (changePlayer p) h) allMovements
    giveCheck = filter (\mv -> isCheck b (changePlayer p) h) allMovements
    toAttack = filter isAttack allMovements
    
    matrixMovements = matrix 8 8 (\pos -> if isPiece b pos && not (isOpposite b p pos) then getMoves b pos h ++ getAttacks b pos h else [])
    allMovements = concat (toList matrixMovements)
