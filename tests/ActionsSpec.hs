module ActionsSpec where

import Data.Matrix
import Test.Hspec

import Actions
import Init
import Simulation
import Structure

initBoard = matrix 8 8 starting
hs = [Movement (7,5) (5,5) (Piece Pawn White) [PawnDoubleMove],
      Movement (2,5) (4,5) (Piece Pawn Black) [PawnDoubleMove],
      Movement (8,6) (5,3) (Piece Bishop White) [],
      Movement (1,2) (3,3) (Piece Knight Black) [],
      Movement (8,4) (4,8) (Piece Queen White) [],
      Movement (1,7) (3,6) (Piece Knight Black) [],
      Movement (4,8) (2,6) (Piece Queen White) [IsAttack]]
g = applyMovements initBoard hs

spec :: Spec
spec = context "Chess Test" gprTest

gprTest :: Spec
gprTest =
  it "4 Move CheckMate" $ do
    --putStrLn "\n"
    --putStrLn (printableMatrix g [] [] [] [] [] [])
    
    isCheck g Black [] `shouldBe` True
    isCheckMate g Black [] `shouldBe` True
