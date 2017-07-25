#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Pandoc.JSON
import Control.Exception
import Data.Monoid ((<>))
import Data.List (partition, isInfixOf)

main :: IO ()
main = toJSONFilter clean

clean :: Block -> [Block]
clean (Plain          []) = []
clean (Para           []) = []
clean (LineBlock      []) = []
clean (BlockQuote     []) = []
clean (OrderedList _  []) = []
clean (BulletList     []) = []
clean (DefinitionList []) = []
clean x                   = [x]

