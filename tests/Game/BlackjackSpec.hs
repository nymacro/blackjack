module Game.BlackjackSpec where

import           Test.Hspec

import           Game.Blackjack

spec :: Spec
spec = do
  describe "Blackjack" $ do
    describe "pickWinner" $ do
      it "should handle empty list" $ do
        pickWinner [] `shouldBe` Nothing

    describe "sumHand" $ do
      it "should handle empty list" $ do
        sumHand [] `shouldBe` []
      it "should sum correctly" $ do
        sumHand [Card Ace Spades, Card Two Hearts] `shouldBe` ([3, 12] :: [Int])
