module Game.Blackjack where

import           Control.Monad
import           Data.Foldable (maximumBy)
import           Data.Functor
import           Data.List     (partition, sort, sortBy)
import           Data.Maybe    (mapMaybe)
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

-- | Helper function to get value of card using its rank
cardValue :: Card -> [Int]
cardValue (Card rank _) = value rank

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

valueCombinations :: Hand -> [[Int]]
valueCombinations x =
  let values = fmap cardValue x
  in combinations values

-- | Take a hand and sum the possible combinations
sumHand :: Hand  -- ^ Hand
        -> [Int] -- ^ Sum of possible combinations
sumHand [] = []
sumHand h =
  let possible = valueCombinations h
  in sum <$> possible

-- | Whether a hand is bust or not
bust :: Hand -> Bool
bust []   = False
bust hand = all (> 21) (sumHand hand)

-- | Take a list of hands, and return a tuple of the winning
-- sum, the combinations possible for the hand and the hand
-- itself.
pickWinner :: [Hand]        -- ^ Hands to choose winner from
           -> [(Int, Hand)] -- ^ Return triple of winning number, card values and hand
pickWinner = pickWinner_ id

-- | Value of best hand (assuming the hand isn't busted)
best :: Hand        -- ^ Hand to find best value of
     -> Maybe [Int] -- ^ Best card value combinations
best hand = let values x = valueCombinations hand
                noBust x = case filter (\x -> sum x <= 21) x of
                             [] -> Nothing
                             x  -> Just x
                z = reverse . sort <$> noBust (values hand)
            in case z of
                 Nothing     -> Nothing
                 Just (x:xs) -> Just x

-- | Get the best value combination for a hand
bestValue :: Hand -> Maybe Int
bestValue x = sum <$> best x

-- | Compare two hands and see which is better
handCompare :: Hand -> Hand -> Ordering
handCompare a b
  | bust a && bust b = EQ
  | bust a = LT
  | bust b = GT
  | blackjack a && blackjack b = EQ
  | blackjack a = GT
  | blackjack b = LT
  | otherwise = let ba = bestValue a
                    bb = bestValue b
                in compare ba bb

-- | Whether a hand is blackjack or not
blackjack :: Hand -> Bool
blackjack hand = case best hand of
                    Nothing -> False
                    Just y  -> length y == 2 && (sum y == 21)

-- | Like pickWinner, but maps the hand from another structure
-- TODO handle "natural 21" as the top winning case
pickWinner_ :: (a -> Hand) -- ^ Mapping function to Hand
            -> [a]        -- ^ Array of items to map to Hands
            -> [(Int, a)] -- ^ Tuple of best value combination, and the given a
pickWinner_ f hands =
  let srt = reverse . sortBy (\(a, _) (b, _) -> a `handCompare` b) $ fmap (\x -> (f x, x)) hands
      fmt w = flip mapMaybe w $ \(x, a) -> let b = bestValue x
                                          in case b of
                                               Nothing -> Nothing
                                               Just y  -> Just (y, a)
      flt = case srt of
              [] -> []
              (x@(a, _):xs) -> let b = bestValue a
                       in case b of
                            Nothing -> []
                            Just y  -> x : takeWhile (\(y, _) -> ((fst x) `handCompare` y) == EQ) xs
  in fmt flt

-- | Play the dealers hand
playDealer :: Hand -> Deck -> (Hand, Deck)
playDealer hand deck =
  case bestValue hand of
     Nothing -> (hand, deck)
     Just n  -> if n > 17
                 then (hand, deck)
                 else let Just (card, deck') = tap deck
                      in playDealer (card : hand) deck'
