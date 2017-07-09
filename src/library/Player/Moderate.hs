module Player.Moderate
  ( whichMove
  ) where

import Data.List
import Data.Maybe
import Data.Matrix
import System.Random

import Actions
import Structure
import Util

value :: Type -> Int
value Pawn = 1
value Knight = 3
value Bishop = 3
value Rook = 5
value Queen = 9
value _ = 0

getBoardValue :: Board -> Player -> Int--(Int,Int)
getBoardValue b p = fst fold - snd fold
  where
    fold = foldl f (0,0) b
    f (x,y) place = if isJust (piece place) then if player (get place) /= p then (x,y+(value (typ (get place)))) else (x+(value (typ (get place))),y) else (x,y)
    get place = fromMaybe (error "") (piece place)

moveOrAttack :: Board -> Movement -> Board
moveOrAttack m mv = if isAttack mv then attack m mv else move m mv

whichMove :: RandomGen t => Board -> Player -> History -> t -> Movement
whichMove b p h g = if null choosenList then err else choosenList !! (rand choosenList)
  where
    err = error ("player " ++ show p ++ " no moves")
    rand list = fst (randomR (0::Int, (length list - 1)::Int) g)
    choosenList
      | not (null winGame) = winGame
      | otherwise = [fst myMv]
    
    winGame = filter (\mv -> isCheckMate (moveOrAttack b mv) (changePlayer p) h) allMovements
    
    getBV init mv = getBoardValue (moveOrAttack init mv) p
    
    enemyMovements = map (\mv -> (mv,getEnemyMoves mv)) allMovements
    getEnemyMoves mv = let nb = moveOrAttack b mv in let h2 = mv:h in concat (toList (matrix 8 8 (\pos -> if isPiece nb pos && isOpposite nb p pos then getMoves nb pos h2 ++ getAttacks nb pos h2 else [])))
    chooseBestEnemyMove =
      mapMaybe (\(mv,emv) -> if null emv then Nothing else Just (mv,minimumBy (\emv1 emv2 -> compare (getBV (moveOrAttack b mv) emv1) (getBV (moveOrAttack b mv) emv2)) emv)) enemyMovements
    myMv = maximumBy (\(mv1,emv1) (mv2,emv2) -> compare (getBV (moveOrAttack b mv1) emv1) (getBV (moveOrAttack b mv2) emv2)) chooseBestEnemyMove
    
    matrixMovements = matrix 8 8 (\pos -> if isPiece b pos && not (isOpposite b p pos) then getMoves b pos h ++ getAttacks b pos h else [])
    allMovements = concat (toList matrixMovements)
