#!/usr/bin/env runhaskell

import Text.Pandoc.JSON
import Text.Pandoc.Generic (topDown)

import Text.Pandoc.Util.Filter.Cols

main :: IO ()
main = toJSONFilter (topDown cols :: Pandoc -> Pandoc)
