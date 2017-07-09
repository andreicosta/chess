module Player.Offensive
  ( whichMove
  ) where

import Data.List
import Data.Matrix
import System.Random

import Actions
import Player.Common
import Structure
import Util

whichMove :: RandomGen t => Board -> Player -> History -> t -> Movement
whichMove b p h g = if null choosenList then err else choosenList !! rand choosenList
  where
    err = error ("player " ++ show p ++ " no moves")
    rand list = fst (randomR (0::Int, (length list - 1)::Int) g)
    choosenList
      | not (null winGame) = winGame
      | not (null giveCheck) = giveCheck
      | not (null allAttacks) = [betterAttack]
      | otherwise = allMovements
    
    winGame = filter (\mv -> isCheckMate (moveOrAttack b mv) (changePlayer p) h) allMovements
    giveCheck = filter (\mv -> isCheck (moveOrAttack b mv) (changePlayer p) h) allMovements
    
    allAttacks = filter isAttack allMovements
    --sortedAttacks = sortBy (\x y -> compare (getBV b x p) (getBV b y p)) allAttacks
    betterAttack = maximumBy (\x y -> compare (getBV b x p) (getBV b y p)) allAttacks
    
    matrixMovements = matrix 8 8 (\pos -> if isPiece b pos && not (isOpposite b p pos) then getMoves b pos h ++ getAttacks b pos h else [])
    allMovements = concat (toList matrixMovements)
