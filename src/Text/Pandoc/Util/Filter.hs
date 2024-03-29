module Text.Pandoc.Util.Filter
    ( attToString
    , convertToStyle
    , revealjsSpecialAttrs
    , revealjsRewriteAttr
    , classToRevealAttr
    , toHtml
    , toBlockHtml
    , addToAtt
    , addToStyle
    )
   where

import Text.Pandoc.Definition
import Data.Monoid
import Data.List (partition, isInfixOf)

-- | adds a given String to the list if not in there; Does nothing if the
--   given String is already present.
addToAtt :: Eq a => a -> [a] -> [a]
addToAtt toAdd (a:as)
  | a == toAdd    = toAdd:as
  | otherwise     = a:addToAtt toAdd as
addToAtt toAdd [] = [toAdd]

-- | adds given String to List of key-value-pairs (like in 'Attr')
--   in the \"style\"-Key.
--
--   Useful when trying to add CSS-styles directly to (generated) elements
addToStyle :: String -> [(String, String)] -> [(String, String)]
-- we are looking for style and inject
addToStyle toAdd (("style",val):as) = ("style", if toAdd `isInfixOf` val then val else val <> " " <> toAdd):as
-- if we land here the current one is not style -> skip
addToStyle toAdd (a:as)             = a:addToStyle toAdd as
-- if we land here we have no more to skip -> add
addToStyle toAdd []                 = [("style", toAdd)]

-- | converts Attributes to String for usage in HTML
--
-- Also converts @width=xxx@ and @height=xxx@ to the
-- corresponding style-attributes
attToString :: Attr -> String
attToString ("", classes, kvpairs) = "class=\"" <> unwords classes <> "\" " <> unwords ((\(k,v) -> k <> "=\"" <> v <> "\"") <$> kvpairs')
  where
    kvpairs' = convertToStyle ["width","height","transform"] kvpairs
attToString (id', classes, kvpairs) = "id=\"" <> id'  <> "\" class=\"" <> unwords classes <> "\" " <> unwords ((\(k,v) -> k <> "=\"" <> v <> "\"") <$> kvpairs')
  where
    kvpairs' = convertToStyle ["width","height","transform"] kvpairs

-- | helper function for 'attToString', but can also be used
--   if you want to extract styles from kv-pair
convertToStyle :: [String] -> [(String,String)] -> [(String,String)]
convertToStyle keys kvpairs = ("style", newstyle):rest
  where
    oldstyle = case filter (\(k,_) -> k == "style") kvpairs of
                 [(_,st)] -> st
                 _        -> ""
    stylesToAdd = filter (\(k,_) -> k    `elem`          keys)  kvpairs
    rest        = filter (\(k,_) -> k `notElem` ("style":keys)) kvpairs
    newstyle = concat ((\(k,v) -> k <> ":" <> v <> ";") <$> stylesToAdd) <> oldstyle

-- | revealjs has some special attributes that has to be
--   passed to the html, but Pandoc only allows
--   @key=value@-attributes, so we have to abuse
--   @.class@ to rewrite them.
--
--   The classes that get rewritten are listed here.
--
--   You probably want 'classToRevealAttr', as that
--   is a wrapper for splitting the class-attribute
revealjsSpecialAttrs :: [String]
revealjsSpecialAttrs = 
    [ "data-markdown"
    , "data-timing"
    , "data-template"
    , "data-autoplay"
    , "data-prevent-swipe"
    , "data-background-interactive"
    , "data-trim"
    , "data-noescape"
    , "data-ignore"
    , "controls"
    , "loop"
    , "muted"
    ]

-- | revealjs has some special attributes that has to be
--   passed to the html, but Pandoc only allows
--   @key=value@-attributes, so we have to abuse
--   @.class@ to rewrite them.
--
--   This is a wrapper-function which just splits the list
--   into real classes and 'revealjsSpecialAttrs'
classToRevealAttr :: [String] -> ([String],[String])
classToRevealAttr = partition (`elem` revealjsSpecialAttrs)

-- | HTML allows for some attributes (i.e. autoplay)
--   for which revealjs offers a special version
--   (i.e. only autoplaying on active slide).
--   These are the things that get rewritten
revealjsRewriteAttr :: [String] -> [String]
revealjsRewriteAttr = fmap replace
  where
    replace :: String -> String
    replace a = case filter ((==a) . fst) replacements of
                  [(_,b)] -> b
                  _       -> a
    replacements :: [(String, String)]
    replacements = [ ("autoplay", "data-autoplay")
                   ]

-- | small wrapper around @RawInline (Format "html")@
--   as this is less line-noise in the filters and the
--   intent is more clear.
toHtml :: String -> Inline
toHtml = RawInline (Format "html")


-- | small wrapper around @Raw (Format "html")@
--   as this is less line-noise in the filters and the
--   intent is more clear.
toBlockHtml :: String -> Block
toBlockHtml = RawBlock (Format "html")
