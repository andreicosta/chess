module Util
  ( postMoveEffects
  , changePlayer
  ) where

import Data.Maybe
import Data.Matrix

import Structure

postMoveEffects :: Matrix Place -> Matrix Place
postMoveEffects = pawnOnFinal

pawnOnFinal :: Matrix Place -> Matrix Place
pawnOnFinal m = matrix 8 8 $ \(x,y) -> if isPawnOnFinal x y then Place (Just (updatePiece x y)) else getElem x y m
  where
    getPiece x y = fromMaybe (error "error: pawnOnFinal: getPiece") (piece (getElem x y m))
    isPawnOnFinal x y =
      isJust(piece (getElem x y m)) &&
      (x == 1 && typ (getPiece x y) == Pawn && player (getPiece x y) == Structure.White ||
       x == 8 && typ (getPiece x y) == Pawn && player (getPiece x y) == Structure.Black)
    
    updatePiece x y = (getPiece x y) {typ = Queen}

changePlayer :: Player -> Player
changePlayer p = if p == White then Black else White
