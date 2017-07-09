module UtilSpec where

import Data.Matrix
import Test.Hspec

import Actions
import Init
import Structure
import Util

initBoard = matrix 8 8 starting

spec :: Spec
spec = context "Util Functions Test" gprTest

gprTest :: Spec
gprTest = do
  it "pawn promotion" $ do
    let board2 = attack initBoard (Movement (2,1) (8,1) (Piece Pawn Black) [])
        board3 = attack initBoard (Movement (7,5) (1,3) (Piece Pawn White) [])
    
    listPawnPromotion initBoard `shouldBe` []
    listPawnPromotion board2 `shouldBe` [(8,1)]
    listPawnPromotion board3 `shouldBe` [(1,3)]
    pawnPromotion board2 Queen (8,1) `shouldNotBe` board2
    pawnPromotion board3 Rook (1,3) `shouldNotBe` board3
  
  it "sets" $ do
    setLeft 1 `shouldBe` 1
    setLeft 8 `shouldBe` 7
    setRight 1 `shouldBe` 2
    setRight 8 `shouldBe` 8
    setUp 1 `shouldBe` 1
    setUp 8 `shouldBe` 7
    setDown 1 `shouldBe` 2
    setDown 8 `shouldBe` 8
  
  it "change player" $ do
    changePlayer White `shouldBe` Black
    changePlayer Black `shouldBe` White
  
  it "getPiece" $ do
    getPiece initBoard (1,1) `shouldBe` (Piece Rook Black)
    getPiece initBoard (8,8) `shouldBe` (Piece Rook White)
    getPiece initBoard (2,1) `shouldBe` (Piece Pawn Black)
  
  it "isPiece" $ do
    isPiece initBoard (1,1) `shouldBe` True
    isPiece initBoard (4,4) `shouldBe` False
  
  it "isOpposite" $ do
    isOpposite initBoard White (1,1) `shouldBe` True
    isOpposite initBoard Black (2,5) `shouldBe` False
    isOpposite initBoard Black (8,8) `shouldBe` True
  
  it "isType" $ do
    isType initBoard Rook (1,1) `shouldBe` True
    isType initBoard King (1,5) `shouldBe` True
    isType initBoard Pawn (2,5) `shouldBe` True
