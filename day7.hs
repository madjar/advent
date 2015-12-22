#!/usr/bin/env stack
-- stack runghc --package parsec

import Text.Parsec
import qualified Data.Map as M
import Data.Bits

data Wire = ConstWire Int | Wire String deriving Show

data Gate = Const Wire
          | And Wire Wire
          | Or Wire Wire
          | Not Wire
          | LShift Wire Int
          | RShift Wire Int
          deriving Show

main = interact (show . compute "a" . parseCircuit)

parseCircuit :: String -> [(String, Gate)]
parseCircuit = either (error . show) id . parse circuit ""
  where circuit = instruction `sepEndBy` newline <* eof
        instruction = flip (,) <$> gate <* string " -> " <*> (many1 letter)
        wire = try (Wire <$> many1 letter) <|> try (ConstWire <$> natural)
        gate =   try (And <$> wire <* string " AND " <*> wire)
             <|> try (Or <$> wire <* string " OR " <*> wire)
             <|> try (Not <$> (string "NOT " *> wire))
             <|> try (LShift <$> wire <* string " LSHIFT " <*> natural)
             <|> try (RShift <$> wire <* string " RSHIFT " <*> natural)
             <|> (Const <$> wire)
        natural = read <$> many1 digit

compute :: String -> [(String, Gate)] -> Int
compute wire rawCircuit = result M.! wire
   where circuit = M.fromList rawCircuit
         result = M.map eval circuit
         eval :: Gate -> Int
         eval (Const w) = evalW w
         eval (And w1 w2) = evalW w1 .&. evalW w2
         eval (Or w1 w2) = evalW w1 .|. evalW w2
         eval (Not w) =  complement $ evalW w
         eval (LShift w n) = shiftL (evalW w) n
         eval (RShift w n) = shiftR (evalW w) n
         evalW :: Wire -> Int
         evalW (ConstWire n) = n
         evalW (Wire s) = result M.! s
