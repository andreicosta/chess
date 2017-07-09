module Player.Common where

import Data.Maybe

import Actions
import Structure

value :: Type -> Int
value Pawn = 1
value Knight = 3
value Bishop = 3
value Rook = 5
value Queen = 9
value _ = 0

getBoardValue :: Board -> Player -> Int
getBoardValue b p = fst fold - snd fold
  where
    fold = foldl f (0,0) b
    f (x,y) place = if isJust (piece place) then if player (get place) /= p then (x,y+(value (typ (get place)))) else (x+(value (typ (get place))),y) else (x,y)
    get place = fromMaybe (error "") (piece place)

moveOrAttack :: Board -> Movement -> Board
moveOrAttack m mv = if isAttack mv then attack m mv else move m mv

getBV :: Board -> Movement -> Player -> Int
getBV init mv p = getBoardValue (moveOrAttack init mv) p
