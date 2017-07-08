module Util
  ( postMoveEffects
  , changePlayer
  , getPiece
  , isPiece
  , isOpposite
  , isType
  , setLeft
  , setRight
  , setUp
  , setDown
  ) where

import Data.Maybe
import Data.Matrix

import Structure

postMoveEffects :: Board -> Board
postMoveEffects = pawnPromotion

-- for while it only promotes to queen
pawnPromotion :: Board -> Board
pawnPromotion m = matrix 8 8 $ \pos@(x,y) -> if isPawnOnFinal pos then Place (Just (updatePiece pos)) else getElem x y m
  where
    isPawnOnFinal pos@(x,_) =
      isPiece m pos &&
      (x == 1 && isType m Pawn pos && isOpposite m Black pos ||
       x == 8 && isType m Pawn pos && isOpposite m White pos)
    
    updatePiece pos = (getPiece m pos) {typ = Queen}

changePlayer :: Player -> Player
changePlayer p = if p == White then Black else White

getPiece :: Board -> Pos -> Piece
getPiece m (x,y) = fromMaybe (error "error: getPiece") (piece (getElem x y m))

isPiece :: Board -> Pos -> Bool
isPiece m (x,y) = isJust (piece (getElem x y m))

isOpposite :: Board -> Player -> Pos -> Bool
isOpposite m p pos = player (getPiece m pos) /= p

isType :: Board -> Type -> Pos -> Bool
isType m t pos = typ (getPiece m pos) == t

setLeft :: Int -> Int
setLeft y = if y == 1 then 1 else y-1

setRight :: Int -> Int
setRight y = if y == 8 then 8 else y+1

setUp :: Int -> Int
setUp x = if x == 1 then 1 else x-1

setDown :: Int -> Int
setDown x = if x == 8 then 8 else x+1
