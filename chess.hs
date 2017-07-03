import Data.Maybe  (isNothing,mapMaybe)
import Data.Matrix (Matrix,matrix,getElem,setElem)

-- Structure

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

-- Initial configurations

starting (1,8) = Place (Just (Piece Rook Black))
starting (1,1) = Place (Just (Piece Rook Black))
starting (1,2) = Place (Just (Piece Knight Black))
starting (1,7) = Place (Just (Piece Knight Black))
starting (1,3) = Place (Just (Piece Bishop Black))
starting (1,6) = Place (Just (Piece Bishop Black))
starting (1,4) = Place (Just (Piece King Black))
starting (1,5) = Place (Just (Piece Queen Black))
starting (2,_) = Place (Just (Piece Pawn Black))

starting (8,8) = Place (Just (Piece Rook White))
starting (8,1) = Place (Just (Piece Rook White))
starting (8,2) = Place (Just (Piece Knight White))
starting (8,7) = Place (Just (Piece Knight White))
starting (8,3) = Place (Just (Piece Bishop White))
starting (8,6) = Place (Just (Piece Bishop White))
starting (8,4) = Place (Just (Piece King White))
starting (8,5) = Place (Just (Piece Queen White))
starting (7,_) = Place (Just (Piece Pawn White))

starting (_,_) = Place Nothing

main = do
  print board9
  return 0

board = matrix 8 8 starting
board2 = walking board (2,5)
board3 = walking board2 (1,2)
board4 = walking board3 (2,2)
board5 = walking board4 (4,2)
board6 = walking board5 (1,1)
board7 = walking board6 (1,3)
board8 = walking board7 (1,5)
board9 = walking board8 (3,5)

walking :: Matrix Place -> (Int,Int) -> Matrix Place
walking m pos@(i,j) = move m pos (last movs)
  where
    elem = getElem i j m
    (Just whatIsThere) = piece elem
    movs = allMoves m pos

-- Fixed Configurations,
-- board independently

-- it's a simplification ...
moves :: Piece -> [Pos]
moves (Piece Pawn Black) = [(1,0)]
moves (Piece Pawn White) = [(-1,0)]
moves (Piece Knight _) = [(i,j) | i <- [-2,-1,1,2], j <- [-2,-1,1,2], i /= j]
moves (Piece Bishop _) = [(i,j) | i <- [-7..7], j <- [-7..7], i /= 0, j /= 0, abs i == abs j]
moves (Piece Rook _) = [(i,j) | i <- [-7..7], j <- [-7..7], i == 0 || j == 0, i /= 0 || j /= 0]
moves (Piece Queen _) = moves (Piece Rook undefined) ++ moves (Piece Bishop undefined)
moves (Piece King _) = [(i,j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]

attacks :: Piece -> [Pos]
attacks (Piece Pawn Black) = [(1,1),(1,-1)]
attacks (Piece Pawn White) = [(-1,1),(-1,-1)]
attacks p = moves p

-- Actions

checkMove :: Matrix Place -> Piece -> (Int,Int) -> (Int,Int) -> Bool
checkMove m (Piece Pawn _) _ (i,j) = isNothing (piece (getElem i j m))
checkMove m (Piece Knight _) _ (i,j) = isNothing (piece (getElem i j m))
checkMove m (Piece King _) _ (i,j) = isNothing (piece (getElem i j m))
checkMove m (Piece Rook _) (ai,aj) (ni,nj) =
  all (\(x,y) -> isNothing (piece (getElem x y m)))
    [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], (x,y) /= (ai,aj)]
checkMove m (Piece Bishop _) (ai,aj) (ni,nj) =
  all (\(x,y) -> isNothing (piece (getElem x y m)))
    [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], abs (x-ai) == abs (y-aj), (x,y) /= (ai,aj)]
checkMove m (Piece Queen p) (ai,aj) (ni,nj) =
  if ai-ni == 0 || aj-nj == 0
    then checkMove m (Piece Rook p) (ai,aj) (ni,nj)
    else checkMove m (Piece Bishop p) (ai,aj) (ni,nj)

move :: Matrix Place -> (Int,Int) -> (Int,Int) -> Matrix Place
move m act@(ai,aj) new@(ni,nj) = m3
  where
    elem = getElem ai aj m
    m2 = setElem (Place Nothing) act m
    m3 = setElem elem (ni,nj) m2

allMoves :: Matrix Place -> (Int,Int) -> [(Int,Int)]
allMoves m pos@(i,j) = filter (checkMove m whatIsThere pos) allBoardMoves
  where
    place = getElem i j m
    (Just whatIsThere) = piece place
    firstPawnMove = typ whatIsThere == Pawn && ((i == 2 && player whatIsThere == Black) || (i == 7 && player whatIsThere == White))
    all = moves whatIsThere ++ (if firstPawnMove then map (\(x,y) -> (x*2,y)) (moves whatIsThere) else [])
    allBoardMoves = mapMaybe (\(x,y) -> if x+i `elem` [1..8] && y+j `elem` [1..8] then Just (x+i,y+j) else Nothing) all
