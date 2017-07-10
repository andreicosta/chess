module Player.Thinker
  ( whichMove
  , whichMoves
  ) where

import Data.List
import System.Random

import Actions
import Player.Common
import Structure
import Util

whichMoves :: Board -> Player -> History -> [Movement]
whichMoves b p h = whichMoves_ b p h 8

whichMove :: RandomGen t => Board -> Player -> History -> t -> Movement
whichMove b p h g = if null choosenList then err else choosenList !! rand choosenList
  where
    err = error ("player " ++ show p ++ " no moves")
    rand list = fst (randomR (0::Int, (length list - 1)::Int) g)
    choosenList = whichMoves b p h

whichMoves_ :: Board -> Player -> History -> Level -> [Movement]
--whichMoves_ b p h l = if length thinkedMoves < 30 then error (show (map snd thinkedMoves)) else choosenList --error ((getTam treeTest))
whichMoves_ b p h l = choosenList --error ((getTam treeTest))
  where
    choosenList
      | not (null winGame) = winGame
      | otherwise = map fst betterMoves
    
    winGame = filter (\mv -> isCheckMate (moveOrAttack b mv) (changePlayer p) h) mvs
    atcks = allAttacks_ b p h
    moves_ = allMoves_ b p h
    mvs = atcks ++ moves_
    
    treeTest = fullTree p l 0 b h
    prefs = searchMovement treeTest
    thinkedMoves = zip mvs prefs
    orderedMoves = sortBy (\(_,v1) (_,v2) -> compare v1 v2) thinkedMoves
    betterMoves = last2 (groupBy (\(_,v1) (_,v2) -> v1 == v2) orderedMoves)

simplifiedMoves_ :: Board -> Player -> History -> [Movement]
simplifiedMoves_ b p h = choosenList
  where
    choosenList
      | not (null winGame) = winGame
      | otherwise = mvs
    
    winGame = filter (\mv -> isCheckMate (moveOrAttack b mv) (changePlayer p) h) mvs
    mvs = allMovements b p h

last2 :: [[t]] -> [t]
last2 l = if null l then [] else last l

type Level = Int
type Value = Int

data Tree = Node Level Board Value [Tree]

--getTam :: Tree -> String
--getTam (Node l _ _ t) = show l ++ ","  ++ show (length t) ++ "," ++ show (sum (map tam t)) ++ ":" ++ concatMap getTam t

--tam :: Tree -> Int
--tam (Node _ _ _ t) = length t + sum (map tam t)

instance Show Tree where
  show (Node _ _ v tree) =
    "(" ++ show v ++ ")" ++ (if null tree then "" else "[" ++ concatMap show tree ++ "]")

fullTree :: Player -> Int -> Level -> Board -> [Movement] -> Tree
fullTree p limit level b h =
  Node level b (getBoardValue2 b p h)
    (map (\mv -> nextTree p limit (level+1) (moveOrAttack b mv) (changePlayer p) (mv:h)) (simplifiedMoves_ b p h))

nextTree :: Player -> Int -> Int -> Board -> Player -> [Movement] -> Tree
nextTree p limit level b act_p h =
  if level > limit-1
--    then
--      if level > limit+1
        then Node level b (getBoardValue2 b p h) []
--        else Node level b (getBoardValue2 b p h) (if null l then [] else [head l])
    else Node level b (getBoardValue2 b p h) l
  where
    l = map (\mv -> nextTree p limit (level+1) (moveOrAttack b mv) (changePlayer act_p) (mv:h)) (whichMoves_ b act_p h 0)

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
