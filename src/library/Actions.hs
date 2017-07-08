module Actions
  ( getAttacks
  , attack
  , getMoves
  , move
  , isCheck
  , isCheckMate
  , undoMovement
  ) where

import Data.Maybe
import Data.Matrix

import Init
import Structure
import Util

-- movements 

checkMove :: Board -> Piece -> Movement -> Bool
checkMove m (Piece Pawn _) move = let (i,j) = target move in isNothing (piece (getElem i j m))
checkMove m (Piece Knight _) move = let (i,j) = target move in isNothing (piece (getElem i j m))
checkMove m (Piece King _) move = let (i,j) = target move in isNothing (piece (getElem i j m))
checkMove m (Piece Rook _) move = let (ai,aj) = source move in let (ni,nj) = target move in
  all (\(x,y) -> isNothing (piece (getElem x y m)))
    [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], (x,y) /= (ai,aj)]
checkMove m (Piece Bishop _) move = let (ai,aj) = source move in let (ni,nj) = target move in
  all (\(x,y) -> isNothing (piece (getElem x y m)))
    [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], abs (x-ai) == abs (y-aj), (x,y) /= (ai,aj)]
checkMove m (Piece Queen p) move = let (ai,aj) = source move in let (ni,nj) = target move in
  if ai-ni == 0 || aj-nj == 0
    then checkMove m (Piece Rook p) move
    else checkMove m (Piece Bishop p) move

checkAttack :: Board -> Piece -> Movement -> Bool
checkAttack m (Piece Pawn p) move =
  isEnPassant move || (isJust (piece (getElem i j m)) && diferentPlayers)
  where
    (i,j) = target move
    toElem = fromMaybe (error "checkAttack: Pawn: toElem") (piece (getElem i j m))
    diferentPlayers = p /= player toElem
checkAttack m (Piece Knight p) move = isJust (piece (getElem i j m)) && diferentPlayers
  where
    (i,j) = target move
    toElem = fromMaybe (error "checkAttack: Knight: toElem") (piece (getElem i j m))
    diferentPlayers = p /= player toElem
checkAttack m (Piece King p) move = isJust (piece (getElem i j m)) && diferentPlayers
  where
    (i,j) = target move
    toElem = fromMaybe (error "checkAttack: King: toElem") (piece (getElem i j m))
    diferentPlayers = p /= player toElem
checkAttack m (Piece Rook p) move = isJust (piece (getElem ni nj m)) && (freePath && diferentPlayers)
  where
    (ai,aj) = source move
    (ni,nj) = target move
    toElem = fromMaybe (error "checkAttack: Rook: toElem") (piece (getElem ni nj m))
    diferentPlayers = p /= player toElem
    freePath =
      all (\(x,y) -> isNothing (piece (getElem x y m)))
        [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], (x,y) /= (ai,aj), (x,y) /= (ni,nj)]
checkAttack m (Piece Bishop p) move = isJust (piece (getElem ni nj m)) && (freePath && diferentPlayers)
  where
    (ai,aj) = source move
    (ni,nj) = target move
    toElem = fromMaybe (error "checkAttack: Bishop: toElem") (piece (getElem ni nj m))
    diferentPlayers = p /= player toElem
    freePath =
      all (\(x,y) -> isNothing (piece (getElem x y m)))
        [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], abs (x-ai) == abs (y-aj), (x,y) /= (ai,aj), (x,y) /= (ni,nj)]
checkAttack m (Piece Queen p) move = let (ai,aj) = source move in let (ni,nj) = target move in
  if ai-ni == 0 || aj-nj == 0
    then checkAttack m (Piece Rook p) move
    else checkAttack m (Piece Bishop p) move

move :: Board -> Movement -> Board
move m movement =
  if isCastling movement then m4 else m3
  where
    act@(ai,aj) = source movement
    new@(_,nj) = target movement
    
    elem = getElem ai aj m
    m2 = setElem (Place Nothing) act m
    m3 = setElem elem new m2
    
    (srcJ,j) = if nj < 5 then (1,4) else (8,6)
    castlingMove = Movement (ai,srcJ) (ai,j) undefined []
    m4 = move m3 castlingMove

attack :: Board -> Movement -> Board
attack m movement = if isEnPassant movement then killEnPassant else kill
  where
    kill = move m movement

    x = if fst (target movement) == 3 then 4 else 5
    (_,y) = target movement
    killEnPassant = setElem (Place Nothing) (x,y) kill

allMoves :: Board -> Pos -> Piece -> History -> [Movement]
allMoves m pos@(i,j) piece h = filter (checkMove m piece) allBoardMoves
  where
    normalMoves = mapMaybe (\(x,y) -> if x+i `elem` [1..8] && y+j `elem` [1..8] then Just (Movement pos (x+i,y+j) piece []) else Nothing) (moves piece)
    castlingMoves = map (\new -> Movement pos new piece [Castling]) (getCastlingMoves m pos piece h)
    doubleStepMoves = map (\(x,y) -> Movement pos (x+i,y+j) piece [PawnDoubleMove]) (pawnDoubleStepMove pos piece)
    allBoardMoves = normalMoves ++ doubleStepMoves ++ castlingMoves

getCastlingMoves :: Board -> Pos -> Piece -> History -> [Pos]
getCastlingMoves m (i,j_) piece history =
  if condKing then rookAvailable 1 ++ rookAvailable 8 else []
  where
    condKing = isType m King (i,j_) && kingNeverMoved && noCheck
    condRook j = isPiece m (i,j) && isType m Rook (i,j) && rookNeverMoved j && freePath j && noPassByAttackedSquare j
    
    playr = player piece
    rookAvailable j = [if j > j_ then (i,j_+2) else (i,j_-2) | condRook j]
    
    kingNeverMoved = all (\(Movement _ _ p _) -> p /= piece) history
    rookNeverMoved j = all (\(Movement src _ _ _) -> src /= (i,j)) history
    noCheck = not (isCheck m playr history)
    freePath j = all (\p -> not (isPiece m (i,p))) (kingPath j)
    noPassByAttackedSquare j = all (\p -> not (isCheck (stepMove p) playr [])) (kingPath j)
    
    stepMove p = move m (Movement (i,j_) (i,p) undefined [])
    kingPath j = if j == 1 then [3,4] else [6,7]

pawnDoubleStepMove :: Pos -> Piece -> [Pos]
pawnDoubleStepMove (i,_) piece = if cond then addDoubleStep else []
  where
    cond = isPawn && inPlace
    isPawn = typ piece == Pawn
    inPlace = (i == 2 && player piece == Black) || (i == 7 && player piece == White)

    addDoubleStep = map (\(x,y) -> (x*2,y)) (moves piece)

allAttacks :: Board -> Pos -> Piece -> History -> [Movement]
allAttacks m pos@(i,j) piece h = filter (checkAttack m piece) allBoardAttacks
  where
    normalAttacks = mapMaybe (\(x,y) -> if x+i `elem` [1..8] && y+j `elem` [1..8] then Just (Movement pos (x+i,y+j) piece [Attack]) else Nothing) (attacks piece)
    enPassantAttacks = map (\new -> Movement pos new piece [EnPassant,Attack]) (enPassant pos piece h)
    allBoardAttacks = normalAttacks ++ enPassantAttacks

enPassant :: Pos -> Piece -> History -> [Pos]
enPassant (i,_) piece history = [(x, y) | cond]
  where
    x = if i == 5 then 6 else 3
    (_,y) = target (head history)

    cond = isPawn && inPlace && enemyPawnDoubleMove
    isPawn = typ piece == Pawn
    inPlace = (i == 5 && player piece == Black) || (i == 4 && player piece == White)
    enemyPawnDoubleMove = lastWasPawnDoubleMove history

getMovements :: (Board -> Pos -> Piece -> History -> [Movement]) -> Board -> Pos -> History -> [Movement]
getMovements f m pos@(x,y) h = if isNothing (piece place) then [] else filter_ mvs
  where
    place = getElem x y m
    (Just piece_) = piece place
    p = player piece_
    
    mvs = f m pos piece_ h
    filter_ = filter (\new -> not (isCheck (move m new) p h))

getMoves :: Board -> Pos -> History -> [Movement]
getMoves = getMovements allMoves

getAttacks :: Board -> Pos -> History -> [Movement]
getAttacks = getMovements allAttacks

undoMovement :: Board -> Movement -> Board
undoMovement m movement
  | isEnPassant movement = createPawn
  | isCastling movement = restoreRook
  | otherwise = undoMove
  where
    piece = getPiece m (target movement)
    
    backSrc = source movement
    backTgt = target movement
    
    x = fst backSrc
    y = snd backTgt
    
    p' = changePlayer (player piece)
    createPawn = setElem (Place (Just (Piece Pawn p'))) (x,y) undoMove
    
    (aj,nj) = if y == 3 then (4,1) else (6,8)
    restoreRook = move undoMove (Movement (x,aj) (x,nj) (Piece Rook (player piece)) [])

    undoMove = move m (Movement backTgt backSrc undefined [])

------------ Second Part
---- Check

-- | Returns `True` if player `p` is under a check
isCheck :: Board -> Player -> History -> Bool
isCheck m p h = if null kingList then error "error: isCheck" else not (null threat)
  where
    king = head kingList
    kingList = concat (toList (matrix 8 8 isMyKing))
    isMyKing pos = [pos | isPiece m pos && not (isOpposite m p pos) && isType m King pos]
    
    threat =
      concatMap (\t ->
        concatMap
          (\mv -> let pos = target mv in [pos | isPiece m pos && isOpposite m p pos && isType m t pos])
        (allAttacks m king (Piece t p) h)
      ) allTypes

-- | Returns `True` if player `p` is under a checkmate
isCheckMate :: Board -> Player -> History -> Bool
isCheckMate m p h = all (==True) (concat (toList checkFreeMovements))
  where
    checkFreeMovements = matrix 8 8 verifyCheck
    verifyCheck pos = if isPiece m pos && not (isOpposite m p pos) then map moveIsCheck (movements pos) else []
    movements pos = getAttacks m pos h ++ getMoves m pos h
    moveIsCheck movement = isCheck (move m movement) p h
