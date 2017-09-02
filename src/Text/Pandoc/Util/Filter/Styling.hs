{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Util.Filter.Styling
    (styling, inlineStyling)
        where

import Text.Pandoc.JSON
import Data.Monoid ((<>))
import Text.Pandoc.Util.Filter
import Prelude hiding (div, span)

-- | Block-Styling
--
--   Special cases captured:
--
--   - #col turns a div into a floating-div for multiple columns
--   - CodeBlock gets attributes @data-trim@ and @data-noescape@
--     automatically
--   - .fragment and .frame work properly on divs
styling :: Block -> IO [Block]
styling (Div ("col",att,att') inner)   = return $ [toBlockHtml $ "<div style=\"float:left; margin-bottom:10px;\"" <> unwords direct <> attToString ("",css,att') <> ">"]
                                                        ++ inner
                                                        ++ [toBlockHtml"</div>"]
                                        where
                                                        (direct, css) = classToRevealAttr att
styling (CodeBlock (id',att,att') inner)   = return
                            [CodeBlock (id', addToAtt "data-trim"
                                           . addToAtt "data-noescape"
                                           $ att
                                           , att')
                                       inner]
styling div@(Div (id',att,att') inner)
  | "fragment" `elem` att = return [Div (id', att, addToStyle "display: block;" att') inner]
  | "frame" `elem` att    = return [Div (id', addToAtt "fragment"        --insert fragment
                                           . addToAtt "current-visible" --insert current-visible
                                           . filter (/= "frame")        --remove frame
                                           $ att
                                           , addToStyle "display: block;" att') inner]
  | otherwise             = return [div]
styling x = return [x]


-- | Inline-Styling
--
--   Special cases captured:
--
--   - .fragment and .frame work properly on spans
--   - .vspace inside span adds a vertical space with @height=xxx@
--   - .hspace inside span adds a horizontal space with @width=xxx@
inlineStyling :: Inline -> Inline
inlineStyling span@(Span (id', att, att') inner)
  | "fragment" `elem` att = Span (id', att, addToStyle "display: inline-block;" att') inner
  | "frame" `elem` att    = Span (id', addToAtt "fragment"        --insert fragment
                                    . addToAtt "current-visible" --insert current-visible
                                    . filter (/= "frame")        --remove frame
                                    $ att
                                    , addToStyle "display: inline-block;" att') inner
  | id' == "vspace"        = toHtml $ "<div style=\"clear:both;\"" <> unwords direct <> attToString ("",css,att') <> "></div>"
  | id' == "hspace"        = toHtml $ "<span " <> unwords direct <> attToString ("",css,att') <> "></span>"
  | otherwise             = span
                                where
                                        (direct, css) = classToRevealAttr att
inlineStyling x = x

