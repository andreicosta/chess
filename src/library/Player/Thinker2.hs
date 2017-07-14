module Player.Thinker2
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
whichMoves b p h = whichMoves_ b p h

whichMove :: RandomGen t => Board -> Player -> History -> t -> Movement
whichMove b p h g = if null choosenList then err else choosenList !! rand choosenList
  where
    err = error ("player " ++ show p ++ " no moves")
    rand list = fst (randomR (0::Int, (length list - 1)::Int) g)
    choosenList = whichMoves b p h

whichMoves_ :: Board -> Player -> History -> [Movement]
whichMoves_ b p h = choosenList
  where
    choosenList
      | not (null winGame) = error ("winGame " ++ (show winGame))--winGame
      
      | not (null (attackingQueen b)) = error ("saveQueen " ++ (show saveQueen)) --saveQueen
      
      | not (null attackThreatenedPieces) = error ("attackThreatenedPieces " ++ (show (attackThreatenedPieces))) --attackThreatenedPieces
      | not (null moveThreatenedPieces) = error ("moveThreatenedPieces " ++ (show (moveThreatenedPieces))) --moveThreatenedPieces
      
      | not (null notIdiotAttacks) = error ("notIdiotAttacks " ++ (show notIdiotAttacks)) --notIdiotAttacks
      | not (null notIdiotThreat) = error ("notIdiotThreat " ++ (show (selectThreat notIdiotThreat))) --selectThreat notIdiotThreat
      
      | not (null castling) = error ("castling " ++ (show (castling))) --castling
      
      | not (null justIdiotAttacks) = error ("justIdiotAttacks " ++ (show (justIdiotAttacks))) --justIdiotAttacks
      | not (null idiotThreat) = error ("idiotThreat " ++ (show (selectThreat idiotThreat))) --selectThreat idiotThreat
      
      -- | not (null veryIdiotAttacks) = error ("veryIdiotAttacks " ++ (show (veryIdiotAttacks))) --veryIdiotAttacks
      
      | otherwise = error ("mvs " ++ (show (mvs))) --mvs
    
    winGame = filter (\mv -> isCheckMate (moveOrAttack b mv) (changePlayer p) h) mvs

    attackingQueen board = [at | at <- allAttacks_ board (changePlayer p) h, typ (getAttacked at) == Queen]
    saveQueen_ = [mv | mv <- mvs, typ (movementPiece mv) == Queen, null (attackingQueen (moveOrAttack b mv))]
    betterAttack_ = sortBy (\mv1 mv2 -> compare (value (typ (movementPiece mv1))) (value (typ (movementPiece mv2)))) (filter isAttack saveQueen_)
    betterAttack = head_ (groupBy (\mv1 mv2 -> (value (typ (movementPiece mv1))) == (value (typ (movementPiece mv2)))) betterAttack_)
    saveQueen = if any isAttack saveQueen_ then betterAttack else saveQueen_

    (myThreatenedPieces_,myThreatenedAndUnprotectedPieces_) = partition (\mv -> null (unprotected b p h (target mv))) (allAttacks_ b (changePlayer p) h)
    myThreatenedPieces = map target myThreatenedPieces_
    myThreatenedAndUnprotectedPieces = map target myThreatenedAndUnprotectedPieces_
    
    myThreatenedPieces__ mv = filter (\mv_ -> null (unprotected (moveOrAttack b mv) p (mv_:mv:h) (target mv_))) (allAttacks_ (moveOrAttack b mv) (changePlayer p) (mv:h))
    attackThreatenedPieces = [mv | mv <- atcks, source mv `elem` myThreatenedAndUnprotectedPieces, value (typ (movementPiece mv)) < value (typ (getAttacked mv))]
    moveThreatenedPieces_ = [mv | mv <- moves_, source mv `elem` myThreatenedPieces, null (unprotected (moveOrAttack b mv) p (mv:h) (target mv))]
    moveThreatenedPieces = [mv | mv <- moveThreatenedPieces_, length (myThreatenedPieces__ mv) < length myThreatenedPieces]
    
    piecesIThreat = atcks
    (notIdiotAttacks__,idiotAttacks) = partition (\mv -> null (unprotected_ (moveOrAttack b mv) p (mv:h) (target mv))) piecesIThreat
    notIdiotAttacks_ = sortBy (\at1 at2 -> compare (value (typ (getAttacked at1))) (value (typ (getAttacked at2)))) notIdiotAttacks__
    notIdiotAttacks = last_ (groupBy (\at1 at2 -> (value (typ (getAttacked at1))) == (value (typ (getAttacked at2)))) notIdiotAttacks_)
    (justIdiotAttacks,veryIdiotAttacks) = partition (\mv -> value (typ (movementPiece mv)) < value (typ (getAttacked mv))) idiotAttacks
    
    makeThreat = filter (\m -> not (null ((allAttacks_ (moveOrAttack b m) p (m:h)) \\ (allAttacks_ b p h)) )) moves_
    (notIdiotThreat,idiotThreat) = partition (\mv -> null (unprotected (moveOrAttack b mv) p (mv:h) (target mv))) makeThreat
    selectThreat_ threats = sortBy (\mv1 mv2 -> compare (value (typ (movementPiece mv1))) (value (typ (movementPiece mv2)))) threats
    selectThreat threats = head_ (groupBy (\mv1 mv2 -> (value (typ (movementPiece mv1))) == (value (typ (movementPiece mv2)))) (selectThreat_ threats))
    
    castling = filter isCastling moves_
    
    atcks = allAttacks_ b p h
    moves_ = allMoves_ b p h
    mvs = atcks ++ moves_

unprotected :: Board -> Player -> History -> Pos -> [Movement]
unprotected b p h pos = filter (\at -> target at == pos && all (\at2 -> target at2 /= pos) (allAttacks_ (moveOrAttack b at) p (at:h))) (allAttacks_ b (changePlayer p) h)

unprotected_ :: Board -> Player -> History -> Pos -> [Movement]
unprotected_ b p h pos = filter (\at -> target at == pos) (allAttacks_ b (changePlayer p) h)
