#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Pandoc.JSON
import Text.Pandoc.Util.Filter.Quiz

main :: IO ()
main = toJSONFilter quiz
