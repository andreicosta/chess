module Structure where

import Data.Maybe
import Data.Matrix

type File = Int
type Rank = Int

type Pos = (Rank,File)

data Movement = Movement
  { source         :: Pos
  , target         :: Pos
  , movementPiece  :: Piece
  , pawnDoubleMove :: Bool
  , isEnPassant    :: Bool
  , isAttack       :: Bool
  } deriving(Eq)

instance Show Movement where
  show (Movement s t _ f1 f2 f3) =
    show s ++ " -> " ++ show t ++ (if f1 then " pdm" else "") ++
    (if f2 then " ep" else "") ++ (if f3 then " at" else "")

lastWasPawnDoubleMove :: History -> Bool
lastWasPawnDoubleMove (h:_) = pawnDoubleMove h
lastWasPawnDoubleMove _ = False

getLastMovement :: History -> (Pos,Pos)
getLastMovement (h:_) = (source h, target h)
getLastMovement _ = error "there is no past."

addHistory :: History -> Movement -> History
addHistory h m = m : h

type History = [Movement]

data Player = White | Black deriving(Eq)

instance Show Player where
  show White = colorCyan ++ "w" ++ colorWhite
  show Black = colorVividBlack ++ "b" ++ colorWhite

data Type = Pawn | Queen | King | Rook | Bishop | Knight deriving(Eq)

allTypes :: [Type]
allTypes = [Pawn,Queen,King,Rook,Bishop,Knight]

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
  } deriving(Eq)

instance Show Piece where
  show (Piece t p) = show t ++ show p

data Place = Place
  { piece :: Maybe Piece
  } deriving(Eq)

instance Show Place where
  show (Place (Just piece)) = show piece
  show (Place Nothing) = "  "

type Board = Matrix Place

colorWhite :: String
colorWhite = "\x1b[39m"

colorVividBlack :: String
colorVividBlack = "\x1b[1;30m"

colorCyan :: String
colorCyan = "\x1b[36m"

-- Print Matrix
printableMatrix :: Board -> String -> [Pos] -> String -> [Pos] -> String -> [Pos] -> String
printableMatrix m colorPiece piecePoints colorPoints movePoints colorAttack attackPoints =
  concatMap (\x -> concatMap (\y -> print x y ++ " ") [1..8] ++ "\n") [1..8]
  where
    printPiece x y = if isNothing (piece (getElem x y m)) then colorPiece ++ "()" ++ colorWhite else printColorPiece x y
    --to diff attackPieces: printColorPiece x y = if (x,y) `elem` attackPoints then colorPiece ++ show (getElem x y m) ++ colorWhite else colorPiece ++ show (getElem x y m) ++ colorWhite
    printColorPiece x y = colorPiece ++ show (getElem x y m) ++ colorWhite
    
    print x y = if (x,y) `elem` piecePoints then printPiece x y else printMove x y
    printMove x y = if (x,y) `elem` movePoints then colorPoints ++ "--" ++ colorWhite else printAttack x y
    printAttack x y = if (x,y) `elem` attackPoints then colorAttack ++ show (getElem x y m) ++ colorWhite else show (getElem x y m)
