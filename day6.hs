#!/usr/bin/env stack
-- stack runghc --package parsec
import Data.Ix
import Text.Parsec
import Data.Either
import Debug.Trace
import Data.List

main = interact (show . sum . applyLines . lines)
--main = interact (show . map parseInstruction . lines)

applyLines = light . map parseInstruction

grid = ((0,0), (999, 999))

data Action = On | Off | Toggle deriving Show
type Instruction = (Action, (Pos, Pos))
type Pos = (Int, Int)

parseInstruction :: String -> Instruction
parseInstruction = either (error . show) id . parse instruction ""
  where instruction = (,) <$> action <* char ' ' <*> posRange
        action = (try $ string "turn on" *> return On)
             <|> (try $ string "turn off" *> return Off)
             <|> (string "toggle" *> return Toggle)
        posRange = (,) <$> pos <* string " through " <*> pos
        pos = (,) <$> natural <* char ',' <*> natural
        natural = read <$> many1 digit

light :: [Instruction] -> [Int]
light instructions = map (\p -> foldl' (flip $ applyInstruction p) 0 instructions) (range grid)
  where applyInstruction :: Pos -> Instruction -> Int -> Int
        applyInstruction p (a, r) = if (inRange r p) then apply a else id
        apply On n = n + 1
        apply Off n = max (n - 1) 0
        apply Toggle n = n + 2
