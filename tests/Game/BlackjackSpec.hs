module Game.BlackjackSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Game.Blackjack

spec :: Spec
spec = do
  describe "Blackjack" $ do
    describe "pickWinner" $ do
      it "should handle empty list" $ do
        pickWinner [] `shouldBe` []
      it "should pick winner correctly" $ do
        let hand1 = [Card Ace Spades, Card Two Hearts]
            hand2 = [Card Three Hearts, Card Five Clubs]
        pickWinner [hand1, hand2] `shouldBe` [(13, [3, 13], hand1)]
        pickWinner [hand1, hand1] `shouldBe` [(13, [3, 13], hand1), (13, [3, 13], hand1)]
        pickWinner [hand1, hand1, hand2] `shouldBe` [(13, [3, 13], hand1), (13, [3, 13], hand1)]
      it "should return Nothing on all bust" $ do
        let bust = [Card King Spades, Card King Clubs, Card King Hearts]
        pickWinner [bust] `shouldBe` []

    describe "tap" $ do
      it "should handle empty list" $ do
        tap [] `shouldBe` Nothing
      it "should take from top of  deck" $ do
        tap [Card Two Hearts] `shouldBe` Just (Card Two Hearts, [])
        tap [Card Two Hearts, Card Three Spades] `shouldBe` Just (Card Two Hearts, [Card Three Spades])

    describe "sumHand" $ do
      it "should handle empty list" $ do
        sumHand [] `shouldBe` []
      it "should sum correctly" $ do
        sumHand [Card Ace Spades, Card Two Hearts] `shouldBe` ([3, 13] :: [Int])

    describe "bust" $ do
      it "should handle empty list" $ do
        bust [] `shouldBe` False
      it "should handle non-bust" $ do
        bust [Card King Hearts] `shouldBe` False
        bust [Card Ace Hearts, Card Jack Spades] `shouldBe` False
        bust [Card Ace Hearts, Card Jack Spades, Card Jack Spades] `shouldBe` False
      it "should bust" $ do
        bust [Card King Hearts, Card King Spades, Card Two Diamonds] `shouldBe` True

    describe "shuffle" $ do
      it "should handle empty list" $ do
        Game.Blackjack.shuffle ([] :: [Int]) [] >>= (`shouldBe` [])

    describe "combinations" $ do
      it "should handle empty list" $ do
        combinations ([] :: [[Int]]) `shouldBe` []
      it "should do correct combinations" $ do
        combinations [[1]] `shouldBe` [[1]]
        combinations [[1], [2]] `shouldBe` [[1, 2]]
        combinations [[1], [2], [3]] `shouldBe` [[1, 2, 3]]
        combinations [[1], [2, 3]] `shouldBe` [[1, 2], [1, 3]]
        combinations [[1, 10], [2, 3]] `shouldBe` [[1, 2], [1, 3], [10, 2], [10, 3]]
