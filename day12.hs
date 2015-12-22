#!/usr/bin/env stack
-- stack runghc --package lens-aeson --package lens --package unordered-containers
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Monoid
import Data.HashMap.Strict
import Debug.Trace

main = interact (show . sumNumbers)


sumNumbers = sumOf (_Value . cosmosOf withoutRed . _Integer)

withoutRed :: Traversal' Value Value
withoutRed =  plate . filtered (notElemOf members "red")
