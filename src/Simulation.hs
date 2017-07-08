module Simulation
  ( applyMovements
  ) where

import Actions
import Structure

applyMovements :: Board -> History -> Board
applyMovements = foldl apply
  where
    apply m mv = 
      if isAttack mv
        then attack m mv
        else move m mv
