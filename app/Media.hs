#!/usr/bin/env runhaskell
import Text.Pandoc.Util.Filter.Media
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter media
