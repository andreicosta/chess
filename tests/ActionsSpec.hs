module ActionsSpec where

import Data.Matrix
import Test.Hspec

import Actions
import Init
import Simulation
import Structure
import Util

initBoard = matrix 8 8 starting

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

spec :: Spec
spec = context "Chess Test" gprTest

gprTest :: Spec
gprTest = do
  it "four moves checkmate" $ do
    let moves = [Movement (7,5) (5,5) (Piece Pawn White) [PawnDoubleMove],
                 Movement (2,5) (4,5) (Piece Pawn Black) [PawnDoubleMove],
                 Movement (8,6) (5,3) (Piece Bishop White) [],
                 Movement (1,2) (3,3) (Piece Knight Black) [],
                 Movement (8,4) (4,8) (Piece Queen White) [],
                 Movement (1,7) (3,6) (Piece Knight Black) [],
                 Movement (4,8) (2,6) (Piece Queen White) [Attack (Piece Pawn Black)]]
        board = applyMovementsTest initBoard [] moves
    
    isCheck board White moves `shouldBe` False
    isCheckMate board White moves `shouldBe` False
    isCheck board Black moves `shouldBe` True
    isCheckMate board Black moves `shouldBe` True
  
  it "castling" $ do
    let starting (1,1) = Place (Just (Piece Rook Black))
        starting (1,5) = Place (Just (Piece King Black))
        starting (1,8) = Place (Just (Piece Rook Black))
        starting (3,7) = Place (Just (Piece Queen Black))
        starting (8,1) = Place (Just (Piece Rook White))
        starting (8,5) = Place (Just (Piece King White))
        starting (8,8) = Place (Just (Piece Rook White))
        starting (_,_) = Place Nothing
        startingBoard = matrix 8 8 starting
        moves = [Movement (1,8) (2,8) (Piece Rook Black) [],
                 Movement (1,1) (1,3) (Piece Rook Black) [],
                 Movement (8,5) (7,4) (Piece King White) [],
                 Movement (1,3) (1,1) (Piece Rook Black) [],
                 Movement (2,8) (1,8) (Piece Rook Black) [],
                 Movement (7,4) (8,5) (Piece King White) []]
        board = applyMovementsTest startingBoard [] moves
  
    all (==False) (map isCastling (getMoves board (1,5) moves)) `shouldBe` True
    all (==False) (map isCastling (getMoves board (8,5) moves)) `shouldBe` True
    
    let c1 = Movement (1,5) (1,3) (Piece King Black) [Castling]
        c2 = Movement (1,5) (1,7) (Piece King Black) [Castling]
        c3 = Movement (8,5) (8,3) (Piece King White) [Castling]
    filter isCastling (getMoves board (1,5) []) `shouldBe` [c1,c2]
    filter isCastling (getMoves board (8,5) []) `shouldBe` [c3]
    
    let castlingMove = head (filter isCastling (getMoves board (1,5) []))
        castl = move board castlingMove
        undo = undoMovement castl castlingMove
    
    undo `shouldBe` board
  
  it "en passant" $ do
    let moves1 = [Movement (7,5) (5,5) (Piece Pawn White) [PawnDoubleMove],
                  Movement (5,5) (4,5) (Piece Pawn White) [],
                  Movement (2,2) (4,2) (Piece Pawn Black) [PawnDoubleMove]]
        moves2 = moves1 ++ [Movement (2,4) (4,4) (Piece Pawn Black) [PawnDoubleMove]]
        history1 = reverse moves1
        history2 = reverse moves2
        board1 = applyMovementsTest initBoard [] moves1
        board2 = applyMovementsTest initBoard [] moves2
    
    let attacks = getAttacks board2 (4,5) history2
    
    null (getAttacks board1 (4,5) history1) `shouldBe` True
    attacks `shouldBe` [Movement (4,5) (3,4) (Piece Pawn White) [EnPassant,Attack (Piece Pawn Black)]]
    
    let enPassant = head attacks
        board3 = attack board2 enPassant
        history3 = enPassant : history2
        undo = foldl undoMovement board3 history3
    
    undo `shouldBe` initBoard
