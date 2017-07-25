#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Control.Exception
import Data.Monoid ((<>))
import Data.List (partition, isInfixOf)

main :: IO ()
main = toJSONFilter $ styling . walk inlineStyling

styling :: Block -> IO [Block]
styling (Div ("col",att,att') inner)   = return $ [toHtml $ "<div style=\"float:left; margin-bottom:10px;\"" <> unwords direct <> attToString ("",css,att') <> ">"]
                                                        ++ inner
                                                        ++ [toHtml"</div>"]
                                        where
                                                        (direct, css) = classToPlain att
styling (CodeBlock (id,att,att') inner)   = return $ [CodeBlock (id, addToAtt "data-trim"
                                                         . addToAtt "data-noescape"
                                                         $ att
                                                           , att') inner]
styling div@(Div (id,att,att') inner)
  | "fragment" `elem` att = return [Div (id, att, addToStyle "display: block;" att') inner]
  | "frame" `elem` att    = return [Div (id, addToAtt "fragment"        --insert fragment
                                           . addToAtt "current-visible" --insert current-visible
                                           . filter (/= "frame")        --remove frame
                                           $ att
                                             , addToStyle "display: block;" att') inner]
  | otherwise             = return [div]
styling x = return [x]


inlineStyling :: Inline -> Inline
inlineStyling span@(Span (id, att, att') inner)
  | "fragment" `elem` att = Span (id, att, addToStyle "display: inline-block;" att') inner
  | "frame" `elem` att    = Span (id, addToAtt "fragment"        --insert fragment
                                    . addToAtt "current-visible" --insert current-visible
                                    . filter (/= "frame")        --remove frame
                                    $ att
                                      , addToStyle "display: inline-block;" att') inner
  | id == "vspace"        = toInlineHtml $ "<div style=\"clear:both;\"" <> unwords direct <> attToString ("",css,att') <> "></div>"
  | id == "hspace"        = toInlineHtml $ "<span " <> unwords direct <> attToString ("",css,att') <> "></span>"
  | otherwise             = span
                                where
                                        (direct, css) = classToPlain att
inlineStyling x = x

addToStyle :: String -> [(String, String)] -> [(String, String)]
-- we are looking for style and inject
addToStyle toAdd (("style",val):as) = ("style", if toAdd `isInfixOf` val then val else val <> " " <> toAdd):as
-- if we land here the current one is not style -> skip
addToStyle toAdd (a:as)             = a:addToStyle toAdd as
-- if we land here we have no more to skip -> add
addToStyle toAdd []                 = [("style", toAdd)]

addToAtt :: String -> [String] -> [String]
addToAtt toAdd (a:as)
  | a == toAdd    = toAdd:as
  | otherwise     = a:addToAtt toAdd as
addToAtt toAdd []           = [toAdd]

attToString :: Attr -> String
attToString (ident, [], kvpairs) = ident <> " " <> unwords ((\(k,v) -> k <> "=\"" <> v <> "\"") <$> kvpairs)
attToString (ident, classes, kvpairs) = ident <> " class=\"" <> unwords classes <> "\" " <> unwords ((\(k,v) -> k <> "=\"" <> v <> "\"") <$> kvpairs)

classToPlain :: [String] -> ([String],[String])
classToPlain = partition (`elem` [ "data-autoplay"
                                 , "controls"
                                 ]
                         )

toHtml :: String -> Block
toHtml = RawBlock (Format "html")

toInlineHtml :: String -> Inline
toInlineHtml = RawInline (Format "html")
