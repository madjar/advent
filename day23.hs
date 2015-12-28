#!/usr/bin/env stack
-- stack runghc --package trifecta --package lens
{-# LANGUAGE TemplateHaskell #-}

import Text.Trifecta
import Control.Lens
import Control.Lens.Extras
import Control.Applicative
import qualified Data.Map as Map

data Register = A | B deriving (Show, Eq, Ord)
data Instruction = Half Register
                 | Triple Register
                 | Increment Register
                 | Jump Integer
                 | JumpIfEven Register Integer
                 | JumpIfOne Register Integer
                 deriving Show


data State = State { _ir :: Integer
                   , _registers :: Map.Map Register Integer
                   } deriving Show

makeLenses ''State

type Program = Map.Map Integer Instruction

main = do Just input <- parseFromFile parser "input23"
          print $ execute (makeProgram input) initialState

parser = many instruction <* eof
instruction = choice [
                symbol "hlf" >> Half <$> register
              , symbol "tpl" >> Triple <$> register
              , symbol "inc" >> Increment <$> register
              , symbol "jmp" >> Jump <$> integer
              , symbol "jie" >> JumpIfEven <$> register <* symbol "," <*> integer
              , symbol "jio" >> JumpIfOne <$> register <* symbol "," <*> integer
              ]

register = (symbol "a" >> pure A) <|> (symbol "b" >> pure B)



initialState = State 0 (Map.fromList [(A, 1), (B, 0)])

makeProgram :: [Instruction] -> Program
makeProgram = Map.fromList . zip [0..]

run :: Instruction -> State -> State
run (Half r) = (registers . ix r %~ (`div` 2)) . tick
run (Triple r) = (registers . ix r *~ 3) . tick
run (Increment r) = (registers . ix r +~ 1) . tick
run (Jump o) = ir +~ o
run (JumpIfEven r o) = \state -> (if even (state ^?! registers . ix r)
                                   then ir +~ o
                                   else tick) state
run (JumpIfOne r o) = \state -> (if 1 == (state ^?! registers . ix r)
                                   then ir +~ o
                                   else tick) state

tick = ir +~ 1

step :: Program -> State -> Maybe State
step program state = run <$> instruction <*> pure state
  where instruction = Map.lookup (state ^?! ir) program

execute :: Program -> State -> [State]
execute program state = case step program state of
                          Just s -> s:execute program s
                          Nothing -> []
