module Actions (getAttacks,attack,getMoves,move) where

import Data.Maybe
import Data.Matrix

import Init
import Structure

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

move :: Matrix Place -> (Int,Int) -> (Int,Int) -> Matrix Place
move m act@(ai,aj) new@(ni,nj) = m3
  where
    elem = getElem ai aj m
    m2 = setElem (Place Nothing) act m
    m3 = setElem elem (ni,nj) m2

attack = move

allMoves :: Matrix Place -> (Int,Int) -> [(Int,Int)]
allMoves m pos@(i,j) = filter (checkMove m whatIsThere pos) allBoardMoves
  where
    place = getElem i j m
    (Just whatIsThere) = piece place
    firstPawnMove = typ whatIsThere == Pawn && ((i == 2 && player whatIsThere == Black) || (i == 7 && player whatIsThere == White))
    all = moves whatIsThere ++ (if firstPawnMove then map (\(x,y) -> (x*2,y)) (moves whatIsThere) else [])
    allBoardMoves = mapMaybe (\(x,y) -> if x+i `elem` [1..8] && y+j `elem` [1..8] then Just (x+i,y+j) else Nothing) all

allAttacks :: Matrix Place -> (Int,Int) -> [(Int,Int)]
allAttacks m pos@(i,j) = filter (checkAttack m whatIsThere pos) allBoardMoves
  where
    place = getElem i j m
    (Just whatIsThere) = piece place
    all = attacks whatIsThere
    allBoardMoves = mapMaybe (\(x,y) -> if x+i `elem` [1..8] && y+j `elem` [1..8] then Just (x+i,y+j) else Nothing) all

getMoves m (x,y) = if isNothing (piece elem) then [] else allMoves m (x,y)
  where
    elem = getElem x y m
    (Just whatIsThere) = piece elem

getAttacks m (x,y) = if isNothing (piece elem) then [] else allAttacks m (x,y)
  where
    elem = getElem x y m
    (Just whatIsThere) = piece elem
