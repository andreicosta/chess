module Structure where

import Data.Maybe
import Data.Matrix

type File = Int
type Rank = Int

type Pos = (Rank,File)

data Info = PawnDoubleMove | EnPassant | Attack Piece | Castling deriving(Eq,Read,Show)

data Movement = Movement
  { source        :: Pos
  , target        :: Pos
  , movementPiece :: Piece
  , info          :: [Info]
  } deriving(Eq,Read,Show)

show_ :: Movement -> String
show_ (Movement s t _ i) =
  show s ++ " -> " ++ show t ++
  concatMap (\f -> " " ++ show f) i

isCastling :: Movement -> Bool
isCastling m = Castling `elem` info m

isEnPassant :: Movement -> Bool
isEnPassant m = EnPassant `elem` info m

isAttack :: Movement -> Bool
isAttack m = any select (info m)
  where
    select (Attack _) = True
    select _ = False

getAttacked :: Movement -> Piece
getAttacked m = if null filt then error "it is not a attack" else piece
  where
    (Attack piece:_) = filt
    filt = filter select (info m)
    select (Attack _) = True
    select _ = False

lastWasPawnDoubleMove :: History -> Bool
lastWasPawnDoubleMove (h:_) = PawnDoubleMove `elem` info h
lastWasPawnDoubleMove _ = False

getLastMovement :: History -> (Pos,Pos)
getLastMovement (h:_) = (source h, target h)
getLastMovement _ = error "there is no past."

addHistory :: History -> Movement -> History
addHistory h m = m : h

type History = [Movement]

data Player = White | Black deriving(Eq,Read,Show)

data Type = Pawn | Queen | King | Rook | Bishop | Knight deriving(Eq,Read,Show)

allTypes :: [Type]
allTypes = [Pawn,Queen,King,Rook,Bishop,Knight]

data Piece = Piece
  { typ :: Type
  , player :: Player
  } deriving(Eq,Read,Show)

data Place = Place
  { piece :: Maybe Piece
  } deriving(Eq)

instance Show Place where
  show (Place (Just piece)) = show_ piece
    where
      show_ (Piece t p) = showT_ t ++ showP_ p
      
      showT_ Pawn = "P"
      showT_ Rook = "R"
      showT_ Bishop = "B"
      showT_ Queen = "Q"
      showT_ King = "K"
      showT_ Knight = "N"
      
      showP_ White = colorCyan ++ "w" ++ colorWhite
      showP_ Black = colorVividBlack ++ "b" ++ colorWhite
      
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
