module Simulation
  ( applyMovements
  , applyMovementsTest
  ) where

import Data.Matrix

import Actions
import Structure

-- | Just apply
applyMovements :: Board -> History -> Board
applyMovements = foldl apply
  where
    apply m mv = 
      if isAttack mv
        then attack m mv
        else move m mv

-- | For each movement, it tests if it is also generated and apply.
applyMovementsTest :: Matrix Place -> History -> [Movement] -> Matrix Place
applyMovementsTest board _ [] = board
applyMovementsTest board h (m:ms) =
  if testFindMoves then nextStep else error "did not found movement"
  where
    applied =
      if isAttack m
        then attack board m
        else move board m
    
    nextStep = applyMovementsTest applied (m:h) ms
    
    generatedMovements =
      getMoves board (source m) h ++
      getAttacks board (source m) h
    testFindMoves = m `elem` generatedMovements
