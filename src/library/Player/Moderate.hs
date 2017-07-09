module Player.Moderate
  ( whichMove
  ) where

import Data.List
import Data.Maybe
import Data.Matrix
import System.Random

import Actions
import Player.Common
import Structure
import Util

whichMove :: RandomGen t => Board -> Player -> History -> t -> Movement
whichMove b p h g = if null choosenList then err else choosenList !! (rand choosenList)
  where
    err = error ("player " ++ show p ++ " no moves")
    rand list = fst (randomR (0::Int, (length list - 1)::Int) g)
    choosenList
      | not (null winGame) = winGame
      | otherwise = map fst myBestMoves
    
    winGame = filter (\mv -> isCheckMate (moveOrAttack b mv) (changePlayer p) h) allMovements
    
    enemyMovements = map (\mv -> (mv,getEnemyMoves mv)) allMovements
    getEnemyMoves mv = let nb = moveOrAttack b mv in let h2 = mv:h in concat (toList (matrix 8 8 (\pos -> if isPiece nb pos && isOpposite nb p pos then getMoves nb pos h2 ++ getAttacks nb pos h2 else [])))
    chooseBestEnemyMove =
      mapMaybe (\(mv,emv) -> if null emv then Nothing else Just (mv,maximumBy (\emv1 emv2 -> compare (getBV (moveOrAttack b mv) emv1 p) (getBV (moveOrAttack b mv) emv2 p)) emv)) enemyMovements
    
    myMoves = sortBy (\(mv1,emv1) (mv2,emv2) -> compare (getBV (moveOrAttack b mv1) emv1 p) (getBV (moveOrAttack b mv2) emv2 p)) chooseBestEnemyMove
    myBestMoves = last (groupBy (\(mv1,emv1) (mv2,emv2) -> (getBV (moveOrAttack b mv1) emv1 p) == (getBV (moveOrAttack b mv2) emv2 p)) myMoves)
    
    matrixMovements = matrix 8 8 (\pos -> if isPiece b pos && not (isOpposite b p pos) then getMoves b pos h ++ getAttacks b pos h else [])
    allMovements = concat (toList matrixMovements)
