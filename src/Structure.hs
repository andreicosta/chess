module Structure where

import Data.Maybe
import Data.Matrix

type Pos = (Int,Int)

data Player = White | Black deriving(Eq)

instance Show Player where
  show White = "w"
  show Black = "b"

-- ignoring Pawns initial double move
--data Init = FirstMove | NormalMove deriving (Eq)

data Type = Pawn | Queen | King | Rook | Bishop | Knight deriving(Eq)

instance Show Type where
  show Pawn = "P"
  show Rook = "R"
  show Bishop = "B"
  show Queen = "Q"
  show King = "K"
  show Knight = "N"

data Piece = Piece
  { typ :: Type
  , player :: Player
  , id :: Int
  } deriving(Eq)

instance Show Piece where
  show (Piece t p _) = show t ++ show p

data Place = Place
  { piece :: Maybe Piece
  } deriving(Eq)

instance Show Place where
  show (Place (Just piece)) = show piece
  show (Place Nothing) = "  "
  
-- Print Matrix
printableMatrix :: Matrix Place -> String -> [Pos] -> String -> [Pos] -> String -> [Pos] -> String
printableMatrix m colorPiece piecePoints colorPoints movePoints colorAttack attackPoints =
  concatMap (\x -> concatMap (\y -> print x y ++ " ") [1..8] ++ "\n") [1..8]
  where
    colorWhite = "\x1b[39m"
    printPiece x y = if isNothing (piece (getElem x y m)) then colorPiece ++ "()" ++ colorWhite else printColorPiece x y
    --to diff attackPieces: printColorPiece x y = if (x,y) `elem` attackPoints then colorPiece ++ show (getElem x y m) ++ colorWhite else colorPiece ++ show (getElem x y m) ++ colorWhite
    printColorPiece x y = colorPiece ++ show (getElem x y m) ++ colorWhite
    
    print x y = if (x,y) `elem` piecePoints then printPiece x y else printMove x y
    printMove x y = if (x,y) `elem` movePoints then colorPoints ++ "--" ++ colorWhite else printAttack x y
    printAttack x y = if (x,y) `elem` attackPoints then colorAttack ++ show (getElem x y m) ++ colorWhite else show (getElem x y m)
