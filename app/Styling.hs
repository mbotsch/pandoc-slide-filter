#!/usr/bin/env runhaskell

import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Pandoc.Util.Filter.Styling

main :: IO ()
main = toJSONFilter $ styling . walk inlineStyling
