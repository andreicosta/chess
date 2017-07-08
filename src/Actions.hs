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
checkMove m (Piece Pawn _ _) move = let (i,j) = target move in isNothing (piece (getElem i j m))
checkMove m (Piece Knight _ _) move = let (i,j) = target move in isNothing (piece (getElem i j m))
checkMove m (Piece King _ _) move = let (i,j) = target move in isNothing (piece (getElem i j m))
checkMove m (Piece Rook _ _) move = let (ai,aj) = source move in let (ni,nj) = target move in
  all (\(x,y) -> isNothing (piece (getElem x y m)))
    [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], (x,y) /= (ai,aj)]
checkMove m (Piece Bishop _ _) move = let (ai,aj) = source move in let (ni,nj) = target move in
  all (\(x,y) -> isNothing (piece (getElem x y m)))
    [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], abs (x-ai) == abs (y-aj), (x,y) /= (ai,aj)]
checkMove m (Piece Queen p _) move = let (ai,aj) = source move in let (ni,nj) = target move in
  if ai-ni == 0 || aj-nj == 0
    then checkMove m (Piece Rook p undefined) move
    else checkMove m (Piece Bishop p undefined) move

checkAttack :: Board -> Piece -> Movement -> Bool
checkAttack m (Piece Pawn p _) move =
  isEnPassant move || (isJust (piece (getElem i j m)) && diferentPlayers)
  where
    (i,j) = target move
    toElem = fromMaybe (error "checkAttack: Pawn: toElem") (piece (getElem i j m))
    diferentPlayers = p /= player toElem
checkAttack m (Piece Knight p _) move = isJust (piece (getElem i j m)) && diferentPlayers
  where
    (i,j) = target move
    toElem = fromMaybe (error "checkAttack: Knight: toElem") (piece (getElem i j m))
    diferentPlayers = p /= player toElem
checkAttack m (Piece King p _) move = isJust (piece (getElem i j m)) && diferentPlayers
  where
    (i,j) = target move
    toElem = fromMaybe (error "checkAttack: King: toElem") (piece (getElem i j m))
    diferentPlayers = p /= player toElem
checkAttack m (Piece Rook p _) move = isJust (piece (getElem ni nj m)) && (freePath && diferentPlayers)
  where
    (ai,aj) = source move
    (ni,nj) = target move
    toElem = fromMaybe (error "checkAttack: Rook: toElem") (piece (getElem ni nj m))
    diferentPlayers = p /= player toElem
    freePath =
      all (\(x,y) -> isNothing (piece (getElem x y m)))
        [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], (x,y) /= (ai,aj), (x,y) /= (ni,nj)]
checkAttack m (Piece Bishop p _) move = isJust (piece (getElem ni nj m)) && (freePath && diferentPlayers)
  where
    (ai,aj) = source move
    (ni,nj) = target move
    toElem = fromMaybe (error "checkAttack: Bishop: toElem") (piece (getElem ni nj m))
    diferentPlayers = p /= player toElem
    freePath =
      all (\(x,y) -> isNothing (piece (getElem x y m)))
        [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], abs (x-ai) == abs (y-aj), (x,y) /= (ai,aj), (x,y) /= (ni,nj)]
checkAttack m (Piece Queen p _) move = let (ai,aj) = source move in let (ni,nj) = target move in
  if ai-ni == 0 || aj-nj == 0
    then checkAttack m (Piece Rook p undefined) move
    else checkAttack m (Piece Bishop p undefined) move

move :: Board -> Pos -> Pos -> Board
move m act@(ai,aj) new = m3
  where
    elem = getElem ai aj m
    m2 = setElem (Place Nothing) act m
    m3 = setElem elem new m2

attack :: Board -> Movement -> Board
attack m movement = if isEnPassant movement then killEnPassant else kill
  where
    kill = move m (source movement) (target movement)

    x = if fst (target movement) == 3 then 4 else 5
    (_,y) = target movement
    killEnPassant = setElem (Place Nothing) (x,y) kill

allMoves :: Board -> Pos -> Piece -> History -> [Movement]
allMoves m pos@(i,j) piece _ = filter (checkMove m piece) allBoardMoves
  where
    normalMoves = mapMaybe (\(x,y) -> if x+i `elem` [1..8] && y+j `elem` [1..8] then Just (Movement pos (x+i,y+j) False False False) else Nothing) (moves piece)
    doubleStepMoves = map (\(x,y) -> Movement pos (x+i,y+j) True False False) (pawnDoubleStepMove pos piece)
    allBoardMoves = normalMoves ++ doubleStepMoves

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
    normalAttacks = mapMaybe (\(x,y) -> if x+i `elem` [1..8] && y+j `elem` [1..8] then Just (Movement pos (x+i,y+j) False False True) else Nothing) (attacks piece)
    enPassantAttacks = map (\new -> Movement pos new False True True) (enPassant pos piece h)
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
    filter_ = filter (\new -> not (isCheck (move m pos (target new)) p h))

getMoves :: Board -> Pos -> History -> [Movement]
getMoves = getMovements allMoves

getAttacks :: Board -> Pos -> History -> [Movement]
getAttacks = getMovements allAttacks

undoMovement :: Board -> [Movement] -> Piece -> Board
undoMovement m history piece = if isEnPassant movement then createPawn else undoMove
  where
    movement = head history
    backSrc = source movement
    backTgt = target movement
    
    x = fst backSrc
    y = snd backTgt
    
    p' = changePlayer (player piece)
    createPawn = setElem (Place (Just (Piece Pawn p' undefined))) (x,y) undoMove
    undoMove = move m backTgt backSrc
  

------------ Second Part
---- Check

-- | Returns `True` if player `p` is under a check
isCheck :: Board -> Player -> History -> Bool
isCheck m p h = if null kingList then error "error: isCheck" else not (null threat)
  where
    king = head kingList
    kingList = concat (toList (matrix 8 8 isMyKing))
    isMyKing pos = [pos | isPiece m pos && not (isOpposite m p pos) && isKing m pos]
    
    threat =
      concatMap (\t ->
        concatMap
          (\mv -> let pos = target mv in [pos | isPiece m pos && isOpposite m p pos && isType m t pos])
        (allAttacks m king (Piece t p undefined) h)
      ) allTypes

-- | Returns `True` if player `p` is under a checkmate
isCheckMate :: Board -> Player -> History -> Bool
isCheckMate m p h = all (==True) (concat (toList checkFreeMovements))
  where
    checkFreeMovements = matrix 8 8 verifyCheck
    verifyCheck pos = if isPiece m pos && not (isOpposite m p pos) then map (moveIsCheck pos) (movements pos) else []
    movements pos = map target (getAttacks m pos h ++ getMoves m pos h)
    moveIsCheck old new = isCheck (move m old new) p h

getPiece :: Board -> Pos -> Piece
getPiece m (x,y) = fromMaybe (error "error: getPiece") (piece (getElem x y m))

isPiece :: Board -> Pos -> Bool
isPiece m (x,y) = isJust (piece (getElem x y m))

isOpposite :: Board -> Player -> Pos -> Bool
isOpposite m p pos = player (getPiece m pos) /= p

isType :: Board -> Type -> Pos -> Bool
isType m t pos = typ (getPiece m pos) == t

isKing :: Board -> Pos -> Bool
isKing m pos = typ (getPiece m pos) == King
