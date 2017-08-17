#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Pandoc.JSON
import Text.Pandoc.Definition
import Text.Pandoc.Generic (topDown)
import Control.Exception
import Data.Monoid ((<>))
import Data.List (partition, elem)
import System.FilePath
import Debug.Trace (trace)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

main :: IO ()
main = toJSONFilter (topDown cols :: Pandoc -> Pandoc)

cols :: [Block] -> [Block]
cols (h@(Header 2 attr [Str wa,Space,Str wb]):a:b:rest) =
  outerDiv:rest
    where
      wa' = fromMaybe 1 (readMaybe wa) :: Int
      wb' = fromMaybe 1 (readMaybe wb) :: Int
      total = wa' + wb'
      pa = (100*wa') `div` total
      pb = (100*wb') `div` total
      outerDiv = Div attr [ makeDiv pa a
                          , makeDiv pb b
                          , clearDiv
                          ]
cols (h@(Header 3 attr [Str wa,Space,Str wb,Space,Str wc]):a:b:c:rest) =
  outerDiv:rest
    where
      wa' = fromMaybe 1 (readMaybe wa) :: Int
      wb' = fromMaybe 1 (readMaybe wb) :: Int
      wc' = fromMaybe 1 (readMaybe wc) :: Int
      total = wa' + wb' + wc'
      pa = (100*wa') `div` total
      pb = (100*wb') `div` total
      pc = (100*wc') `div` total
      outerDiv = Div attr [ makeDiv pa a
                          , makeDiv pb b
                          , makeDiv pc c
                          , clearDiv
                          ]
cols x = x

makeDiv :: Int -> Block -> Block
makeDiv width content = Div ("", [], [("style","width:" <> show width <> "%;float:left")]) [content]

clearDiv :: Block
clearDiv = Div ("", [], [("style", "clear: both")]) [Plain [toHtml "&nbsp;"]]

toHtml :: String -> Inline
toHtml = RawInline (Format "html")
