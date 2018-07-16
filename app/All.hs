#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Pandoc.Generic
import Text.Pandoc.Util.Filter.Quiz
import Text.Pandoc.Util.Filter.Cols
import Text.Pandoc.Util.Filter.Media
import Text.Pandoc.Util.Filter.Styling

main :: IO ()
main = toJSONFilter $
    fmap (walk (concatMap clean) :: Pandoc -> Pandoc) .
    (walkM (fmap concat . mapM styling) :: Pandoc -> IO Pandoc) <=<
    (walkM (fmap concat . mapM media) :: Pandoc -> IO Pandoc) .
    (walk (concatMap quiz) :: Pandoc -> Pandoc) .
    (topDown cols :: Pandoc -> Pandoc)

clean :: Block -> [Block]
clean (Plain          []) = []
clean (Para           []) = []
clean (LineBlock      []) = []
clean (BlockQuote     []) = []
clean (OrderedList _  []) = []
clean (BulletList     []) = []
clean (DefinitionList []) = []
clean x                   = [x]

