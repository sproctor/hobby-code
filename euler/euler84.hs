import Data.List

-- This program doesn't find the answer. I just guessed at it.

possibilities = 36

rolls :: [(Int, Double)]
rolls =
  map (\(x,y) -> (x,(fromIntegral y) / (fromIntegral possibilities)))
    [ (2, 1)
    , (3, 2)
    , (4, 3)
    , (5, 4)
    , (6, 5)
    , (7, 6)
    , (8, 5)
    , (9, 4)
    , (10, 3)
    , (11, 2)
    , (12, 1)
    ]

data Odds
  = OddsSq Int Double
  | OddsArr [Odds] Double
  deriving Show

oddsCC pos =
  [ OddsSq 0 0.0625
  , OddsSq 10 0.0625
  , OddsSq pos 0.875]

oddsCH :: Int -> [Odds]
oddsCH pos =
  [ OddsSq 0 0.0625
  , OddsSq 10 0.0625
  , OddsSq 11 0.0625
  , OddsSq 24 0.0625
  , OddsSq 39 0.0625
  , OddsSq 5 0.0625
  , OddsSq nextR 0.125
  , OddsSq nextU 0.0625
  , OddsArr (oddsEnding (pos-3)) 0.0625
  , OddsSq pos 0.375
  ]
  where
    nextR
      | pos > 35 || pos < 5 = 5
      | pos > 5 = 15
      | pos > 15 = 25
      | pos > 25 = 35
    nextU
      | pos < 12 || pos > 28 = 12
      | pos > 12 = 28

oddsEnding :: Int -> [Odds]
oddsEnding pos
  | pos == 2 || pos == 17 || pos == 33 = oddsCC pos
  | pos == 7 || pos == 22 || pos == 36 = oddsCH pos
  | otherwise = [OddsSq pos 1.0]

oddsFromStart :: Int -> [Odds]
oddsFromStart start =
  map (\(x,y) -> OddsArr (oddsEnding (start + x)) y) rolls

flattenOdds :: Odds -> [(Int, Double)]
flattenOdds (OddsSq sq p) = [(sq, p)]
flattenOdds (OddsArr (sq:rest) p) = (map (\(x,y) -> (x,y*p)) $ flattenOdds sq) ++ flattenOdds (OddsArr rest p)
flattenOdds (OddsArr [] _) = []

mergeOdds :: [(Int, Double)] -> [(Int, Double)]
mergeOdds [] = []
mergeOdds (odds@(sq,p):rest) =
  (sq,p') : mergeOdds others
  where
    (matched, others) = partition (\(x,_) -> x == sq) rest
    p' = foldl (\x (_,y) -> x + y) p matched

total [] = 0.0
total ((_,x):rest) = x + total rest
