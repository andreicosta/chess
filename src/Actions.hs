module Actions (getMoves,move) where

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

getMoves m (x,y) = if isNothing (piece elem) then [] else allMoves m (x,y)
  where
    elem = getElem x y m
    (Just whatIsThere) = piece elem
