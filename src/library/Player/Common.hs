module Player.Common where

import Data.Maybe
import Data.Matrix

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

getBoardValue :: Board -> Player -> Int
getBoardValue b p = uncurry (-) (getSumPieces b p)

getSumPieces :: Board -> Player -> (Int, Int)
getSumPieces b p = foldl f (0,0) b
  where
    f (x,y) place =
      if isJust (piece place)
        then if player (get place) /= p
          then (x,y + value (typ (get place)))
          else (x + value (typ (get place)),y)
        else (x,y)
    get place = fromMaybe (error "") (piece place)

getBoardValue2 :: Board -> Player -> History -> Int
getBoardValue2 b p h = fst fold*1000 + sum myAttacks - snd fold*1000 - sum enemyAttacks
  where
    fold = getSumPieces b p
    
    myAttacks = map (value . typ . getAttacked) (allAttacks_ b p h)
    enemyAttacks = map (value . typ . getAttacked) (allAttacks_ b (changePlayer p) h)

moveOrAttack :: Board -> Movement -> Board
moveOrAttack m mv = if isAttack mv then attack m mv else move m mv

getBV :: Board -> Movement -> Player -> Int
getBV init mv = getBoardValue (moveOrAttack init mv)

allMovements :: Board -> Player -> History -> [Movement]
allMovements b p h =
  concat (toList
    (matrix 8 8
      (\pos ->
         if isPiece b pos && not (isOpposite b p pos)
           then getMoves b pos h ++ getAttacks b pos h
           else [])))

allMoves_ :: Board -> Player -> History -> [Movement]
allMoves_ b p h =
  concat (toList
    (matrix 8 8
      (\pos ->
         if isPiece b pos && not (isOpposite b p pos)
           then getMoves b pos h
           else [])))

allAttacks_ :: Board -> Player -> History -> [Movement]
allAttacks_ b p h =
  concat (toList
    (matrix 8 8
      (\pos ->
         if isPiece b pos && not (isOpposite b p pos)
           then getAttacks b pos h
           else [])))
