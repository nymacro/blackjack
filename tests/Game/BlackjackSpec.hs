module Game.BlackjackSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Game.Blackjack

spec :: Spec
spec = do
  describe "Blackjack" $ do
    describe "handCompare" $ do
      let natural = [Card Ace Hearts, Card King Spades]
      it "should allow equality" $ do
        handCompare [] [] `shouldBe` EQ
        handCompare [Card Ace Spades] [Card Ace Hearts] `shouldBe` EQ
        handCompare [Card Ace Spades] [Card Seven Hearts, Card Four Hearts] `shouldBe` EQ
        handCompare natural natural `shouldBe` EQ
      it "should favour natural blackjack" $ do
        handCompare natural [Card Ace Spades, Card Five Hearts, Card Five Clubs] `shouldBe` GT
        handCompare [Card Ace Spades, Card Five Hearts, Card Five Clubs] natural `shouldBe` LT

    describe "best" $ do
      it "should pick best value" $ do
        let hand1 = [Card Ace Spades]
            hand2 = [Card Ace Spades, Card King Hearts]
            hand3 = [Card King Hearts, Card Five Clubs]
        best hand1 `shouldBe` Just [11]
        best hand2 `shouldBe` Just [11, 10]
        best hand3 `shouldBe` Just [10, 5]
      it "should handle bust" $ do
        best [Card King Hearts, Card Queen Hearts, Card Ten Spades] `shouldBe` Nothing
      it "should handle edge cases" $ do
        best [] `shouldBe` Nothing
      it "should handle edge" $ do
        bestValue [Card Ten Diamonds, Card Nine Diamonds] `shouldBe` Just 19
        bestValue [Card Ten Diamonds, Card Nine Diamonds, Card Three Hearts] `shouldBe` Nothing

    describe "pickWinner" $ do
      it "should handle empty list" $ do
        pickWinner [] `shouldBe` []
      it "should pick winner correctly" $ do
        let hand1 = [Card Ace Spades, Card Two Hearts]
            hand2 = [Card Three Hearts, Card Five Clubs]
        pickWinner [hand1, hand2] `shouldBe` [(13, hand1)]
        pickWinner [hand1, hand1] `shouldBe` [(13, hand1), (13, hand1)]
        pickWinner [hand1, hand1, hand2] `shouldBe` [(13, hand1), (13, hand1)]
      it "should pick winner correctly (blackjack)" $ do
        let hand1 = [Card Ace Spades, Card King Hearts]
            hand2 = [Card Ace Spades, Card Five Hearts, Card Five Clubs]
        pickWinner [hand1, hand2] `shouldBe` [(21, hand1)]
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

    describe "playDealer" $ do
      it "should do the right thing" $ do
        playDealer [Card Queen Clubs] [Card Six Hearts, Card Seven Hearts]
          `shouldBe` ([Card Seven Hearts, Card Six Hearts, Card Queen Clubs], [])
        playDealer [Card King Clubs] [Card Ace Hearts, Card King Diamonds]
          `shouldBe` ([Card Ace Hearts, Card King Clubs], [Card King Diamonds])
        playDealer [Card Jack Spades] [Card Eight Spades,Card Seven Spades]
          `shouldBe` ([Card Eight Spades, Card Jack Spades], [Card Seven Spades])
