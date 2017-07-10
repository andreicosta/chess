module StructureSpec where

import Data.Matrix
import Test.Hspec

import Init
import Structure

initBoard = matrix 8 8 starting

spec :: Spec
spec = context "Board Test" gprTest

gprTest :: Spec
gprTest =
  it "print initial board" $ do
    let c1 = "1"
        c2 = "2"
        c3 = "3"

    printableMatrix initBoard c1 [] c2 [] c3 [] `shouldBe` "R\ESC[1;30mb\ESC[39m N\ESC[1;30mb\ESC[39m B\ESC[1;30mb\ESC[39m Q\ESC[1;30mb\ESC[39m K\ESC[1;30mb\ESC[39m B\ESC[1;30mb\ESC[39m N\ESC[1;30mb\ESC[39m R\ESC[1;30mb\ESC[39m \nP\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m \n                        \n                        \n                        \n                        \nP\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m \nR\ESC[36mw\ESC[39m N\ESC[36mw\ESC[39m B\ESC[36mw\ESC[39m Q\ESC[36mw\ESC[39m K\ESC[36mw\ESC[39m B\ESC[36mw\ESC[39m N\ESC[36mw\ESC[39m R\ESC[36mw\ESC[39m \n"
    printableMatrix initBoard c1 [(2,4)] c2 [(4,4),(6,7)] c3 [(2,2),(1,8)] `shouldBe` "R\ESC[1;30mb\ESC[39m N\ESC[1;30mb\ESC[39m B\ESC[1;30mb\ESC[39m Q\ESC[1;30mb\ESC[39m K\ESC[1;30mb\ESC[39m B\ESC[1;30mb\ESC[39m N\ESC[1;30mb\ESC[39m 3R\ESC[1;30mb\ESC[39m\ESC[39m \nP\ESC[1;30mb\ESC[39m 3P\ESC[1;30mb\ESC[39m\ESC[39m P\ESC[1;30mb\ESC[39m 1P\ESC[1;30mb\ESC[39m\ESC[39m P\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m P\ESC[1;30mb\ESC[39m \n                        \n         2--\ESC[39m             \n                        \n                  2--\ESC[39m    \nP\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m P\ESC[36mw\ESC[39m \nR\ESC[36mw\ESC[39m N\ESC[36mw\ESC[39m B\ESC[36mw\ESC[39m Q\ESC[36mw\ESC[39m K\ESC[36mw\ESC[39m B\ESC[36mw\ESC[39m N\ESC[36mw\ESC[39m R\ESC[36mw\ESC[39m \n"
