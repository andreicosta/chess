module Init where

import Structure

starting (1,8) = Place (Just (Piece Rook Black 1))
starting (1,1) = Place (Just (Piece Rook Black 2))
starting (1,2) = Place (Just (Piece Knight Black 3))
starting (1,7) = Place (Just (Piece Knight Black 4))
starting (1,3) = Place (Just (Piece Bishop Black 5))
starting (1,6) = Place (Just (Piece Bishop Black 6))
starting (1,4) = Place (Just (Piece King Black 7))
starting (1,5) = Place (Just (Piece Queen Black 8))
starting (2,_) = Place (Just (Piece Pawn Black 9))

starting (8,8) = Place (Just (Piece Rook White 10))
starting (8,1) = Place (Just (Piece Rook White 11))
starting (8,2) = Place (Just (Piece Knight White 12))
starting (8,7) = Place (Just (Piece Knight White 13))
starting (8,3) = Place (Just (Piece Bishop White 14))
starting (8,6) = Place (Just (Piece Bishop White 15))
starting (8,4) = Place (Just (Piece King White 16))
starting (8,5) = Place (Just (Piece Queen White 17))
starting (7,_) = Place (Just (Piece Pawn White 18))

starting (_,_) = Place Nothing

-- Alternative starting boards

startingTest (1,1) = Place (Just (Piece Rook White 18))
startingTest (4,4) = Place (Just (Piece Rook White 17))
startingTest (2,4) = Place (Just (Piece Pawn White 15))
startingTest (4,6) = Place (Just (Piece Queen Black 16))
startingTest (3,5) = Place (Just (Piece Pawn Black 1))
startingTest (_,_) = Place Nothing

-- Fixed (board independently) Configurations

-- it's a simplification ...
moves :: Piece -> [Pos]
moves (Piece Pawn Black _) = [(1,0)]
moves (Piece Pawn White _) = [(-1,0)]
moves (Piece Knight _ _) = [(i,j) | i <- [-2,-1,1,2], j <- [-2,-1,1,2], i /= j, abs i + abs j == 3]
moves (Piece Bishop _ _) = [(i,j) | i <- [-7..7], j <- [-7..7], i /= 0, j /= 0, abs i == abs j]
moves (Piece Rook _ _) = [(i,j) | i <- [-7..7], j <- [-7..7], i == 0 || j == 0, i /= 0 || j /= 0]
moves (Piece Queen _ _) = moves (Piece Rook undefined undefined) ++ moves (Piece Bishop undefined undefined)
moves (Piece King _ _) = [(i,j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]

attacks :: Piece -> [Pos]
attacks (Piece Pawn Black _) = [(1,1),(1,-1)]
attacks (Piece Pawn White _) = [(-1,1),(-1,-1)]
attacks p = moves p
