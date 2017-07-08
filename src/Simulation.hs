module Simulation
  ( applyMovements
  ) where

import Actions
import Structure

applyMovements :: Board -> History -> Board
applyMovements = foldl apply
  where
    apply = 
      if True
        then \m (Movement s t _ _ _ _) -> move m s t
        else attack
