#!/usr/bin/env stack
-- stack runghc

import Data.List

data Item = Item {name :: String, cost :: Int, damage :: Int, armor :: Int} deriving (Show, Eq)

weapons = [ Item "Dagger" 8 4 0
          , Item "Shortsword" 10 5 0
          , Item "Warhammer" 25 6 0
          , Item "Longsword" 40 7 0
          , Item "Greataxe" 74 8 0
          ]

armors = [ Item "Leather" 13 0 1
         , Item "Chaimail" 31 0 2
         , Item "Splitmail" 53 0 3
         , Item "Bandedmail" 75 0 4
         , Item "Platemail" 102 0 5]

rings = [ Item "Damage +1" 25 1 0
        , Item "Damage +2" 50 2 0
        , Item "Damage +3" 100 3 0
        , Item "Defense +1" 20 0 1
        , Item "Defense +2" 40 0 2
        , Item "Defense +3" 80 0 3
        ]

bossLife = 109
bossDamage = 8
bossArmor = 2
life = 100

playerWins damage armor = playerStrikes <= bossStrikes
  where hit = max (damage - bossArmor) 1
        bossHit = max (bossDamage - armor) 1
        playerStrikes = strikes bossLife hit
        bossStrikes = strikes life bossHit
        strikes l h =  ((l - 1) `div` h) + 1

inventoryWins i = playerWins (sum $ map damage i) (sum $ map armor i)

possibleWeapons :: [[Item]]
possibleWeapons = map return weapons

possibleArmors = [] : map return armors

possibleRings = (nub . map (take 2) . permutations $ rings) ++ map return rings ++ []

possibleInventories = do
  w <- possibleWeapons
  a <- possibleArmors
  r <- possibleRings
  return (w ++ a ++ r)

inventoryCost = sum . map cost

cheapestWin = find inventoryWins (sortOn inventoryCost possibleInventories)
mostExpensiveLoss = find (not . inventoryWins) (sortOn (negate . inventoryCost) possibleInventories)
