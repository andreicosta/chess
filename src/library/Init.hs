module Init where

import Structure

starting :: Pos -> Place
starting (1,8) = Place (Just (Piece Rook Black))
starting (1,1) = Place (Just (Piece Rook Black))
starting (1,2) = Place (Just (Piece Knight Black))
starting (1,7) = Place (Just (Piece Knight Black))
starting (1,3) = Place (Just (Piece Bishop Black))
starting (1,6) = Place (Just (Piece Bishop Black))
starting (1,4) = Place (Just (Piece Queen Black))
starting (1,5) = Place (Just (Piece King Black))
starting (2,_) = Place (Just (Piece Pawn Black))

starting (8,8) = Place (Just (Piece Rook White))
starting (8,1) = Place (Just (Piece Rook White))
starting (8,2) = Place (Just (Piece Knight White))
starting (8,7) = Place (Just (Piece Knight White))
starting (8,3) = Place (Just (Piece Bishop White))
starting (8,6) = Place (Just (Piece Bishop White))
starting (8,4) = Place (Just (Piece Queen White))
starting (8,5) = Place (Just (Piece King White))
starting (7,_) = Place (Just (Piece Pawn White))

starting (_,_) = Place Nothing

-- Alternative starting boards

startingTest :: Pos -> Place
startingTest (1,1) = Place (Just (Piece Rook White))
startingTest (4,4) = Place (Just (Piece Rook White))
startingTest (2,4) = Place (Just (Piece Pawn White))
startingTest (4,6) = Place (Just (Piece Queen Black))
startingTest (3,5) = Place (Just (Piece Pawn Black))
startingTest (1,6) = Place (Just (Piece King White))
startingTest (_,_) = Place Nothing

startingTest2 :: Pos -> Place
--startingTest2 (1,1) = Place (Just (Piece Rook White))
startingTest2 (2,3) = Place (Just (Piece Queen White))
startingTest2 (3,5) = Place (Just (Piece King White))
startingTest2 (1,1) = Place (Just (Piece King Black))
startingTest2 (_,_) = Place Nothing

-- Fixed (board independently) Configurations

-- it's a simplification ...
moves :: Piece -> [Pos]
moves (Piece Pawn Black) = [(1,0)]
moves (Piece Pawn White) = [(-1,0)]
moves (Piece Knight _) = [(i,j) | i <- [-2,-1,1,2], j <- [-2,-1,1,2], i /= j, abs i + abs j == 3]
moves (Piece Bishop _) = [(i,j) | i <- [-7..7], j <- [-7..7], i /= 0, j /= 0, abs i == abs j]
moves (Piece Rook _) = [(i,j) | i <- [-7..7], j <- [-7..7], i == 0 || j == 0, i /= 0 || j /= 0]
moves (Piece Queen _) = moves (Piece Rook undefined) ++ moves (Piece Bishop undefined)
moves (Piece King _) = [(i,j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]

attacks :: Piece -> [Pos]
attacks (Piece Pawn Black) = [(1,1),(1,-1)]
attacks (Piece Pawn White) = [(-1,1),(-1,-1)]
attacks p = moves p
