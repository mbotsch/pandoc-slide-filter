#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Pandoc.JSON

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

