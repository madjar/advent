#!/usr/bin/env stack
-- stack runghc --package cryptohash

import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as B16

main = print $ head . filter isGood $ [1..]

isGood :: Integer -> Bool
isGood n =  B.pack "000000" `B.isPrefixOf` hashed n

hashed n = B16.encode . hash . B.pack $ "iwrupvqb" ++ show n
