#!/usr/bin/env stack
-- stack runghc --package lens --package heap
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

import Control.Lens
import Control.Monad.State (execState)
import qualified Data.Map as Map
import qualified Data.Heap as H
import Control.Arrow
import Control.Monad

import Debug.Trace

bossDamage = 9
-- bossHitPoint = 51

data Effect = Shield | Poison | Recharge deriving (Show, Eq, Ord)
data Spell = Missile | Drain | Eff Effect deriving (Show)

data State = State { _playerHP :: Integer
                   , _mana :: Integer
                   , _bossHP :: Integer
                   , _effects :: Map.Map Effect Integer
                   , _manaSpent :: Integer
                   , _casted :: [Spell]
                   , _previousState :: Maybe State
--                   , turn :: Turn
                   } deriving Show

makeLenses ''State


--data Turn = Player | Boss deriving Show


mkState player mana boss = State player mana boss mempty 0 [] Nothing
initialState = mkState 50 500 51


allSpells = [Missile, Drain, Eff Shield, Eff Poison, Eff Recharge]

cost :: Spell -> Integer
cost Missile = 53
cost Drain = 73
cost (Eff Shield) = 113
cost (Eff Poison) = 173
cost (Eff Recharge) = 229

duration :: Effect -> Integer
duration Shield = 6
duration Poison = 6
duration Recharge = 5

possible :: Spell -> State -> Bool
possible spell state = cost spell <= (state ^. mana) && not (inEffect spell)
  where inEffect (Eff e) = has (effects . ix e) state
        inEffect _ = False

act :: Spell -> State -> State
act spell = execState (doit >> removeMana)
  where removeMana = do mana -= cost spell
                        manaSpent += cost spell
                        casted <>= [spell]
        doit = case spell of
                 Missile -> bossHP -= 4
                 Drain -> bossHP -= 2 >> playerHP += 2
                 Eff e -> effects . at e ?= duration e

applyEffect :: Effect -> State -> State
applyEffect e = execState (doit >> reduceDuration)
  where reduceDuration = effects . at e %= \case Just 1 -> Nothing
                                                 Just n -> Just (n - 1)
        doit = case e of
                 Shield -> return ()
                 Poison -> bossHP -= 3
                 Recharge -> mana += 101

tick :: State -> State
tick state = (compose $ map applyEffect effs) state
  where effs = Map.keys (state ^. effects)
        compose = foldr (.) id

chooseSpell :: State -> [Spell]
chooseSpell state = filter (\s -> possible s state) allSpells

bossAttack :: State -> State
bossAttack state = execState (playerHP -= (bossDamage - armor)) state
  where armor = if has (effects . ix Shield) state
                  then 7
                  else 0

loseOneHP = execState (playerHP -= 1)

oneTurn :: State -> [State]
oneTurn state = do let state0 = loseOneHP state
                   guard (state0 ^. playerHP > 0)
                   let state1 = tick state0
                   spell <- chooseSpell state1
                   let state2 = bossAttack . tick . act spell $ state1
                   guard (isWon state2 || state2 ^. playerHP > 0)
                   return state2 {_previousState = Just state}

isWon :: State -> Bool
isWon state = state ^. bossHP <= 0

isLost :: State -> Bool
isLost state = not (isWon state) && state ^. playerHP <= 0

solve :: State -> [State]
solve s0 = go (H.singleton (0, s0))
  where go :: H.MinPrioHeap Integer State -> [State]
        go (H.view -> Just ((_, state), rest))
          -- | traceShow (state) False = undefined
          | isWon state = state : go rest
          | isLost state = go rest
          | otherwise = go (rest `H.union` newStates)
              where newStates = H.fromList . map (_manaSpent &&& id) . oneTurn $ state
        go _ = []

solve' = solve initialState

printHist = mapM_ print . hist
hist (Just state) = hist (_previousState state) ++ [state {_previousState = Nothing}]
hist Nothing = []
