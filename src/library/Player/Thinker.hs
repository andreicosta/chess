module Player.Thinker
  ( whichMove
  ) where

import Data.List
import System.Random

import Actions
import Player.Common
import Structure
import Util

whichMove :: RandomGen t => Board -> Player -> History -> t -> Movement
whichMove b p h g = if null choosenList then err else choosenList !! rand choosenList
  where
    err = error ("player " ++ show p ++ " no moves")
    rand list = fst (randomR (0::Int, (length list - 1)::Int) g)
    choosenList
      | not (null winGame) = winGame
      | otherwise = map fst betterMoves
    
    winGame = filter (\mv -> isCheckMate (moveOrAttack b mv) (changePlayer p) h) mvs
    mvs = allMovements b p h
    
    treeTest = fullTree p 2 0 b p h
    prefs = searchMovement treeTest
    thinkedMoves = zip mvs prefs
    orderedMoves = sortBy (\(_,v1) (_,v2) -> compare v1 v2) thinkedMoves
    betterMoves = last (groupBy (\(_,v1) (_,v2) -> v1 == v2) orderedMoves)

type Level = Int
type Value = Int

data Tree = Node Level Board Value [Tree]

instance Show Tree where
  show (Node l _ v tree) =
    "{" ++ show l ++ "," ++ show v ++ "}=("
    ++ concatMap show tree ++ ")"

fullTree :: Player -> Int -> Level -> Board -> Player -> [Movement] -> Tree
fullTree p limit level b act_p h =
  Node level b (getBoardValue b p)
    (map (\mv -> nextTree p limit (level+1) (moveOrAttack b mv) (changePlayer act_p) (mv:h)) (allMovements b act_p h))

enemyMoves :: Board -> Player -> History -> [Movement]
enemyMoves b p h = choosenList_
  where
    choosenList_
      | not (null winGame_) = winGame_
      | otherwise = map fst betterMoves_
    
    winGame_ = filter (\mv -> isCheckMate (moveOrAttack b mv) (changePlayer p) h) mvs_
    mvs_ = allMovements b p h
    
    treeTest_ = fullTree p 0 0 b p h
    prefs_ = searchMovement treeTest_
    thinkedMoves_ = zip mvs_ prefs_
    orderedMoves_ = sortBy (\(_,v1) (_,v2) -> compare v1 v2) thinkedMoves_
    betterMoves_ = last (groupBy (\(_,v1) (_,v2) -> v1 == v2) orderedMoves_)

nextTree :: Player -> Int -> Int -> Board -> Player -> [Movement] -> Tree
nextTree p limit level b act_p h =
  if level > limit
    then Node level b (getBoardValue b p) []
    else Node level b (getBoardValue b p) l
  where
    l = if p == act_p
      then map (\mv -> nextTree p limit (level+1) (moveOrAttack b mv) (changePlayer act_p) (mv:h)) (allMovements b act_p h)
      else map (\mv -> nextTree p limit (level+1) (moveOrAttack b mv) (changePlayer act_p) (mv:h)) (enemyMoves b act_p h)

searchMovement :: Tree -> [Value]
searchMovement (Node _ _ _ t) = map (\t -> searchMovement_ t (-500000)) t

searchMovement_ :: Tree -> Value -> Value
searchMovement_ (Node _ _ v []) vMax =
  if v > vMax
    then v
    else vMax

searchMovement_ (Node a b v (t:ts)) vMax =
  if v > child
    then searchMovement_ (Node a b v ts) vMax
    else searchMovement_ (Node a b child ts) vMax
  where
    child = searchMovement_ t vMax
