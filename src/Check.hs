module Check
  ( isCheck
  , isCheckMate
  ) where

import Data.Maybe
import Data.Matrix

import Actions
import Structure

-- | Returns `True` if player `p` is under a check
isCheck :: Matrix Place -> Player -> Bool
isCheck m p = True `elem` killers 
  where
    killers = matrix 8 8 isKiller
    isKiller pos = isPiece m pos && isOpposite m p pos && canKillKing m myKing pos
    
    myKing = concat (toList (matrix 8 8 isMyKing))
    isMyKing pos = [pos | isPiece m pos && not (isOpposite m p pos) && isKing m pos]

-- | Returns `True` if player `p` is under a checkmate
isCheckMate :: Matrix Place -> Player -> Bool
isCheckMate m p = all (==True) (concat (toList checkFreeMovements))
  where
    checkFreeMovements = matrix 8 8 verifyCheck
    verifyCheck pos = if isPiece m pos && not (isOpposite m p pos) then map (moveIsCheck pos) (movements pos) else []
    movements pos = getAttacks m pos ++ getMoves m pos
    moveIsCheck old new = isCheck (move m old new) p

getPiece :: Matrix Place -> Pos -> Piece
getPiece m (x,y) = fromMaybe (error "error: getPiece") (piece (getElem x y m))

isPiece :: Matrix Place -> Pos -> Bool
isPiece m (x,y) = isJust (piece (getElem x y m))

isOpposite :: Matrix Place -> Player -> Pos -> Bool
isOpposite m p (x,y) = player (getPiece m (x,y)) /= p

isKing :: Matrix Place -> Pos -> Bool
isKing m (x,y) = typ (getPiece m (x,y)) == King

canKillKing :: Matrix Place -> [Pos] -> Pos -> Bool
canKillKing m myKing (x,y) = any (`elem` myKing) (getAttacks m (x,y))
