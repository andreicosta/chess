module Structure where

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
  } deriving(Eq)

instance Show Piece where
  show (Piece t p) = show t ++ show p

data Place = Place
  { piece :: Maybe Piece
  } deriving(Eq)

instance Show Place where
  show (Place (Just piece)) = show piece
  show (Place Nothing) = "  "
