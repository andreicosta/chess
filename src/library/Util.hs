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
pawnPromotion m = matrix 8 8 $ \(x,y) -> if isPawnOnFinal x y then Place (Just (updatePiece x y)) else getElem x y m
  where
    getPiece x y = fromMaybe (error "error: pawnOnFinal: getPiece") (piece (getElem x y m))
    isPawnOnFinal x y =
      isJust(piece (getElem x y m)) &&
      (x == 1 && typ (getPiece x y) == Pawn && player (getPiece x y) == Structure.White ||
       x == 8 && typ (getPiece x y) == Pawn && player (getPiece x y) == Structure.Black)
    
    updatePiece x y = (getPiece x y) {typ = Queen}

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
