module ActionsSpec where

import Data.Matrix
import Test.Hspec

import Actions
import Init
import Simulation
import Structure

initBoard = matrix 8 8 starting

fourMove =
  [Movement (7,5) (5,5) (Piece Pawn White) [PawnDoubleMove],
   Movement (2,5) (4,5) (Piece Pawn Black) [PawnDoubleMove],
   Movement (8,6) (5,3) (Piece Bishop White) [],
   Movement (1,2) (3,3) (Piece Knight Black) [],
   Movement (8,4) (4,8) (Piece Queen White) [],
   Movement (1,7) (3,6) (Piece Knight Black) [],
   Movement (4,8) (2,6) (Piece Queen White) [Attack]]
fourMoveBoard = applyMovements initBoard fourMove

startingCastling (1,1) = Place (Just (Piece Rook Black))
startingCastling (1,5) = Place (Just (Piece King Black))
startingCastling (1,8) = Place (Just (Piece Rook Black))
startingCastling (8,1) = Place (Just (Piece Rook White))
startingCastling (8,5) = Place (Just (Piece King White))
startingCastling (8,8) = Place (Just (Piece Rook White))
startingCastling (_,_) = Place Nothing
initBoardCastling = matrix 8 8 startingCastling
moveCastling =
  [Movement (1,8) (2,8) (Piece Rook Black) [],
   Movement (1,1) (1,3) (Piece Rook Black) [],
   Movement (8,5) (7,4) (Piece King White) [],
   Movement (1,3) (1,1) (Piece Rook Black) [],
   Movement (2,8) (1,8) (Piece Rook Black) [],
   Movement (7,4) (8,5) (Piece King White) []]
boardCastling = applyMovements initBoardCastling moveCastling

spec :: Spec
spec = context "Chess Test" gprTest

gprTest :: Spec
gprTest = do
  it "4 Move CheckMate" $ do
    --putStrLn "\n"
    --putStrLn (printableMatrix fourMoveBoard [] [] [] [] [] [])
    
    isCheck fourMoveBoard Black [] `shouldBe` True
    isCheckMate fourMoveBoard Black [] `shouldBe` True
  
  it "Castling" $ do
    --putStrLn "\n"
    --putStrLn (printableMatrix initBoardCastling [] [] [] [] [] [])
    --putStrLn "\n"
    --putStrLn (show (map info (getMoves boardCastling (1,5) moveCastling)))
    --putStrLn (show (map target (getMoves boardCastling (1,5) moveCastling)))
    --putStrLn (show (map info (getMoves boardCastling (8,5) moveCastling)))
    --putStrLn (show (map target (getMoves boardCastling (8,5) moveCastling)))
    
    all (==False) (map isCastling (getMoves boardCastling (1,5) moveCastling)) `shouldBe` True
    all (==False) (map isCastling (getMoves boardCastling (8,5) moveCastling)) `shouldBe` True
    length (filter isCastling (getMoves boardCastling (1,5) [])) `shouldBe` 2
    length (filter isCastling (getMoves boardCastling (8,5) [])) `shouldBe` 2
    
    let castlingMove = head (filter isCastling (getMoves boardCastling (1,5) []))
        castl = move boardCastling castlingMove
        undo = undoMovement castl castlingMove (Piece King Black)
    
    undo `shouldBe` boardCastling
