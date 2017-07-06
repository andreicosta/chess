module Actions
  ( getAttacks
  , attack
  , getMoves
  , move
  , isCheck
  , isCheckMate
  ) where

import Data.Maybe
import Data.Matrix

import Init
import Structure

-- movements 

checkMove :: Matrix Place -> Piece -> (Int,Int) -> (Int,Int) -> Bool
checkMove m (Piece Pawn _ _) _ (i,j) = isNothing (piece (getElem i j m))
checkMove m (Piece Knight _ _) _ (i,j) = isNothing (piece (getElem i j m))
checkMove m (Piece King _ _) _ (i,j) = isNothing (piece (getElem i j m))
checkMove m (Piece Rook _ _) (ai,aj) (ni,nj) =
  all (\(x,y) -> isNothing (piece (getElem x y m)))
    [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], (x,y) /= (ai,aj)]
checkMove m (Piece Bishop _ _) (ai,aj) (ni,nj) =
  all (\(x,y) -> isNothing (piece (getElem x y m)))
    [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], abs (x-ai) == abs (y-aj), (x,y) /= (ai,aj)]
checkMove m (Piece Queen p _) (ai,aj) (ni,nj) =
  if ai-ni == 0 || aj-nj == 0
    then checkMove m (Piece Rook p undefined) (ai,aj) (ni,nj)
    else checkMove m (Piece Bishop p undefined) (ai,aj) (ni,nj)

checkAttack :: Matrix Place -> Piece -> (Int,Int) -> (Int,Int) -> Bool
checkAttack m (Piece Pawn p _) _ (i,j) = isJust (piece (getElem i j m)) && diferentPlayers
  where
    toElem = fromMaybe (error "checkAttack: Pawn: toElem") (piece (getElem i j m))
    diferentPlayers = p /= player toElem
checkAttack m (Piece Knight p _) _ (i,j) = isJust (piece (getElem i j m)) && diferentPlayers
  where
    toElem = fromMaybe (error "checkAttack: Knight: toElem") (piece (getElem i j m))
    diferentPlayers = p /= player toElem
checkAttack m (Piece King p _) _ (i,j) = isJust (piece (getElem i j m)) && diferentPlayers
  where
    toElem = fromMaybe (error "checkAttack: King: toElem") (piece (getElem i j m))
    diferentPlayers = p /= player toElem
checkAttack m (Piece Rook p _) (ai,aj) (ni,nj) = isJust (piece (getElem ni nj m)) && (freePath && diferentPlayers)
  where
    toElem = fromMaybe (error "checkAttack: Rook: toElem") (piece (getElem ni nj m))
    diferentPlayers = p /= player toElem
    freePath =
      all (\(x,y) -> isNothing (piece (getElem x y m)))
        [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], (x,y) /= (ai,aj), (x,y) /= (ni,nj)]
checkAttack m (Piece Bishop p _) (ai,aj) (ni,nj) = isJust (piece (getElem ni nj m)) && (freePath && diferentPlayers)
  where
    toElem = fromMaybe (error "checkAttack: Bishop: toElem") (piece (getElem ni nj m))
    diferentPlayers = p /= player toElem
    freePath =
      all (\(x,y) -> isNothing (piece (getElem x y m)))
        [(x,y) | x <- [(min ai ni)..(max ai ni)], y <- [(min aj nj)..(max aj nj)], abs (x-ai) == abs (y-aj), (x,y) /= (ai,aj), (x,y) /= (ni,nj)]
checkAttack m (Piece Queen p _) (ai,aj) (ni,nj) =
  if ai-ni == 0 || aj-nj == 0
    then checkAttack m (Piece Rook p undefined) (ai,aj) (ni,nj)
    else checkAttack m (Piece Bishop p undefined) (ai,aj) (ni,nj)

move :: Matrix Place -> Pos -> Pos -> Matrix Place
move m act@(ai,aj) new = m3
  where
    elem = getElem ai aj m
    m2 = setElem (Place Nothing) act m
    m3 = setElem elem new m2

attack :: Matrix Place -> Pos -> Pos -> Matrix Place
attack = move

allMoves :: Matrix Place -> Pos -> Piece -> [Pos]
allMoves m pos@(i,j) whatIsThere = filter (checkMove m whatIsThere pos) allBoardMoves
  where
    firstPawnMove = typ whatIsThere == Pawn && ((i == 2 && player whatIsThere == Black) || (i == 7 && player whatIsThere == White))
    all = moves whatIsThere ++ (if firstPawnMove then map (\(x,y) -> (x*2,y)) (moves whatIsThere) else [])
    allBoardMoves = mapMaybe (\(x,y) -> if x+i `elem` [1..8] && y+j `elem` [1..8] then Just (x+i,y+j) else Nothing) all

allAttacks :: Matrix Place -> Pos -> Piece -> [Pos]
allAttacks m pos@(i,j) whatIsThere = filter (checkAttack m whatIsThere pos) allBoardAttacks
  where
    all = attacks whatIsThere
    allBoardAttacks = mapMaybe (\(x,y) -> if x+i `elem` [1..8] && y+j `elem` [1..8] then Just (x+i,y+j) else Nothing) all

getMovements :: (Matrix Place -> Pos -> Piece -> [Pos]) -> Matrix Place -> Pos -> [Pos]
getMovements f m (x,y) = if isNothing (piece place) then [] else filter_ mvs
  where
    place = getElem x y m
    (Just whatIsThere) = piece place
    p = player whatIsThere
    
    mvs = getMovementsFree f m (x,y)
    filter_ = filter (\new -> not (isCheck (move m (x,y) new) p))

getMovementsFree :: (Matrix Place -> Pos -> Piece -> [Pos]) -> Matrix Place -> Pos -> [Pos]
getMovementsFree f m (x,y) = if isNothing (piece place) then [] else f m (x,y) whatIsThere
  where
    place = getElem x y m
    (Just whatIsThere) = piece place

getMoves :: Matrix Place -> Pos -> [Pos]
getMoves = getMovements allMoves

getAttacks :: Matrix Place -> Pos -> [Pos]
getAttacks = getMovements allAttacks

getAttacksFree :: Matrix Place -> Pos -> [Pos]
getAttacksFree = getMovementsFree allAttacks

getMovesFree :: Matrix Place -> Pos -> [Pos]
getMovesFree = getMovementsFree allMoves




------------ Second Part
---- Check

-- | Returns `True` if player `p` is under a check
isCheck :: Matrix Place -> Player -> Bool
isCheck m p = True `elem` killers 
  where
    killers = matrix 8 8 isKiller
    isKiller pos = isPiece m pos && isOpposite m p pos && canKillKing m myKing pos
    
    myKing = concat (toList (matrix 8 8 isMyKing))
    isMyKing pos = [pos | isPiece m pos && not (isOpposite m p pos) && isKing m pos]

-- | Returns `True` if player `p` is under a checkmate
isCheckMate :: Matrix Place -> Player -> Bool
isCheckMate m p = all (==True) (concat (toList checkFreeMovements))
  where
    checkFreeMovements = matrix 8 8 verifyCheck
    verifyCheck pos = if isPiece m pos && not (isOpposite m p pos) then map (moveIsCheck pos) (movements pos) else []
    movements pos = getAttacksFree m pos ++ getMovesFree m pos
    moveIsCheck old new = isCheck (move m old new) p

getPiece :: Matrix Place -> Pos -> Piece
getPiece m (x,y) = fromMaybe (error "error: getPiece") (piece (getElem x y m))

isPiece :: Matrix Place -> Pos -> Bool
isPiece m (x,y) = isJust (piece (getElem x y m))

isOpposite :: Matrix Place -> Player -> Pos -> Bool
isOpposite m p (x,y) = player (getPiece m (x,y)) /= p

isKing :: Matrix Place -> Pos -> Bool
isKing m (x,y) = typ (getPiece m (x,y)) == King

canKillKing :: Matrix Place -> [Pos] -> Pos -> Bool
canKillKing m myKing (x,y) = any (`elem` myKing) (getAttacks m (x,y))
