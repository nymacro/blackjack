module Game.Blackjack where

import           Control.Monad
import           Data.Foldable (maximumBy)
import           Data.Functor
import           Data.Monoid

import           System.Random

-- | A card's suit
data Suit = Spades
          | Hearts
          | Clubs
          | Diamonds
  deriving (Show, Eq)

-- | A card's color
data Color = Red
           | Black
  deriving (Show, Eq)

-- | The color of a Suit
color :: Suit -> Color
color Spades   = Black
color Clubs    = Black
color Hearts   = Red
color Diamonds = Red

-- | A card's rank
data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
  deriving (Show, Eq, Enum)

-- | The numeric value for the card
value :: Rank -> [Int]
value Jack  = [10]
value Queen = [10]
value King  = [10]
value Ace   = [1, 11]
value x     = [fromEnum x + 2]

-- | A card
data Card = Card Rank Suit
  deriving (Show, Eq)

-- | A card deck
type Deck = [Card]

-- | Return a ordered deck of 52 cards
defaultDeck :: Deck
defaultDeck = deck
  where
    ranks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
    suits = [Spades, Diamonds, Clubs, Hearts]
    deck  = [ Card rank suit
            | rank <- ranks
            , suit <- suits ]

-- | Shuffle deck of cards
shuffleDeck :: Deck -> IO Deck
shuffleDeck = shuffle []

-- | Shuffle a list
-- TODO maybe make this faster
shuffle :: [a]    -- ^ Results accumulator
        -> [a]    -- ^ List to shuffle
        -> IO [a] -- ^ Shuffled list result
shuffle s [] = return s
shuffle shuffled deck = do
  ix <- getStdRandom $ randomR (1, length deck)
  let (front, back') = splitAt (ix - 1) deck
      (card:_, back) = splitAt 1 back'
  shuffle (card : shuffled) (front <> back)

-- | Take a card from the top of the deck
tap :: Deck -> Maybe (Card, Deck)
tap []     = Nothing
tap (x:xs) = Just (x, xs)

-- | A hand of cards
type Hand = [Card]

-- should do equivalent of this for hand values
-- [[2], [1, 10]]
testV :: [[Int]]
testV = do
  x <- [2]
  y <- [1, 10]
  return $ [x] <> [y]

-- | Given a list of combinations (list), return a list of all the
-- possible variations of those combinations.
--
-- E.g. Given [[2], [1, 10]], will return [[2, 1], [2, 10]]
combinations :: [[a]] -> [[a]]
combinations []     = []
combinations values = forM values (>>= return)

-- | Take a hand and sum the possible combinations
sumHand :: Hand  -- ^ Hand
        -> [Int] -- ^ Sum of possible combinations
sumHand [] = []
sumHand h =
  let values   = fmap (\(Card rank _) -> value rank) h
      possible = combinations values
  in sum <$> possible

-- | Whether a hand is bust or not
bust :: Hand -> Bool
bust []   = False
bust hand = all (> 21) (sumHand hand)

-- | Take a list of hands, and return a tuple of the winning
-- sum, the combinations possible for the hand and the hand
-- itself.
pickWinner :: [Hand]               -- ^ Hands to choose winner from
           -> [(Int, [Int], Hand)] -- ^ Return triple of winning number, card values and hand
pickWinner = pickWinner_ id

-- | Like pickWinner, but maps the hand from another structure
-- TODO handle "natural 21" as the top winning case
pickWinner_ :: (a -> Hand)
            -> [a]
            -> [(Int, [Int], a)]
pickWinner_ f hands =
  let values x = (sumHand (f x), x)
      -- filter out hands which have busted
      noBust' (n, _) = if null $ filter (<= 21) n
                         then False
                         else True
      noBust  = filter noBust' (values <$> hands)
      -- get the winning card value
      tmax (ah, _) (bh, _) = compare (sum ah) (sum bh)
      (maxValue, _) = maximumBy tmax noBust
      winners = filter (\(x, _) -> maximum x == maximum maxValue) noBust
  in fmap (\(x, y) -> (maximum x, x, y)) winners
