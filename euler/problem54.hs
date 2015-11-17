import Maybe
import List

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving Eq

data HandRank = HighCard
              | OnePair       Integer
              | TwoPairs      Integer Integer
              | ThreeOfAKind  Integer
              | Straight
              | Flush
              | FullHouse     Integer Integer
              | FourOfAKind   Integer
              | StraightFlush

compareRank :: HandRank -> HandRank -> Ordering
compareRank StraightFlush StraightFlush = EQ
compareRank StraightFlush _             = GT
compareRank _ StraightFlush             = LT
compareRank (FourOfAKind a) (FourOfAKind b) = compare a b
compareRank (FourOfAKind _) _               = GT
compareRank _ (FourOfAKind _)               = LT
compareRank (FullHouse a1 a2) (FullHouse b1 b2)
            | a1 == b1  = compare a2 b2
            | otherwise = compare a1 b1
compareRank (FullHouse _ _) _ = GT
compareRank _ (FullHouse _ _) = GT
compareRank Flush Flush = EQ
compareRank Flush _     = GT
compareRank _ Flush     = LT
compareRank Straight Straight = EQ
compareRank Straight _        = GT
compareRank _ Straight        = LT
compareRank (ThreeOfAKind a) (ThreeOfAKind b) = compare a b
compareRank (ThreeOfAKind _) _                = GT
compareRank _ (ThreeOfAKind _)                = LT
compareRank (TwoPairs a1 a2) (TwoPairs b1 b2)
            | a1 == b2  = compare a2 b2
            | otherwise = compare a1 b1
compareRank (TwoPairs _ _) _ = GT
compareRank _ (TwoPairs _ _) = LT
compareRank (OnePair a) (OnePair b) = compare a b
compareRank (OnePair _) _           = GT
compareRank _ (OnePair _)           = LT
compareRank HighCard HighCard       = EQ

isFlush :: [(Integer, Suit)] -> Bool
isFlush ((_,suit):hand) =
  let
    aux [] = True
    aux ((_,s):rest) = if s == suit then aux rest else False
  in aux hand

isStraight :: [(Integer, Suit)] -> Bool
isStraight hand =
  let
    (values, _) = unzip hand
    (first:sortedValues) = sort values
    aux [] _ = True
    aux (card:rest) prev =
      if card == prev + 1 then aux rest card
                          else False
  in aux sortedValues first

countCards :: [(Integer, Suit)] -> [(Integer, Integer)]
countCards hand =
  let
    (vals, _) = unzip hand
    (first:svals) = sort vals
    aux [] count = count
    aux (card:rest) ((prev,num):count) =
      if card == prev then aux rest ((prev,num+1):count)
                      else aux rest ((card,1):(prev,num):count)
  in aux svals [(first,1)]

isFourOfAKind :: [(Integer, Suit)] -> Maybe Integer
isFourOfAKind hand =
  let
    count = countCards hand
    aux [] = Nothing
    aux ((card,num):rest) =
      if num == 4 then Just card
                  else aux rest
  in aux count

isFullHouse :: [(Integer, Suit)] -> Maybe (Integer, Integer)
isFullHouse hand =
  let
    threeCards = isThreeOfAKind hand
    twoCards   = isOnePair hand
  in if isJust threeCards && isJust twoCards
       then Just ((fromJust threeCards), (fromJust twoCards))
       else Nothing

isThreeOfAKind :: [(Integer, Suit)] -> Maybe Integer
isThreeOfAKind hand =
  let
    count = countCards hand
    aux [] = Nothing
    aux ((card,num):rest) =
      if num == 3 then Just card
                  else aux rest
  in aux count

isOnePair :: [(Integer, Suit)] -> Maybe Integer
isOnePair hand =
  let
    count = countCards hand
    aux [] = Nothing
    aux ((card,num):rest) =
      if num == 2 then Just card
                  else aux rest
  in aux count

isTwoPairs :: [(Integer, Suit)] -> Maybe (Integer, Integer)
isTwoPairs hand =
  let
    count = countCards hand
    firstPair = isOnePair hand
    val = fromJust firstPair
    aux :: [(Integer, Integer)] -> Maybe (Integer, Integer)
    aux [] = Nothing
    aux ((card,num):rest) =
      if num == 2 && card /= val then Just ((max card val), (min card val))
                                 else aux rest
  in if firstPair == Nothing then Nothing
                             else aux count

scoreHand hand
  | flush && straight   = StraightFlush
  | isJust fourOfAKind  = FourOfAKind (fromJust fourOfAKind)
  | isJust fullHouse    = let (a,b) = fromJust fullHouse in FullHouse a b
  | flush               = Flush
  | straight            = Straight
  | isJust threeOfAKind = ThreeOfAKind (fromJust threeOfAKind)
  | isJust twoPairs     = let (a,b) = fromJust twoPairs in TwoPairs a b
  | isJust onePair      = OnePair (fromJust onePair)
  where
    flush        = isFlush hand
    straight     = isStraight hand
    fourOfAKind  = isFourOfAKind hand
    fullHouse    = isFullHouse hand
    threeOfAKind = isThreeOfAKind hand
    twoPairs     = isTwoPairs hand
    onePair      = onePair hand

main = print (compareRank (ThreeOfAKind 10) (StraightFlush))
