#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Pandoc.JSON
import Control.Exception
import Data.Monoid ((<>))
import Data.List (partition)

main :: IO ()
main = toJSONFilter media

media :: Inline -> IO [Inline]
media (Image ("audio",att,att') [] (filename,_))   = return $ [toHtml $ "<audio " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString ("",css,att') <> ">"]
                                                                ++ [toHtml"</audio>"]
                                                        where
                                                                (direct, css) = classToPlain att
media (Image ("audio",att,att') alt (filename,_))   = return $ [toHtml $ "<figure><audio " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString ("",css,att') <> "></audio>"]
                                                                ++ [toHtml $ "<figcaption>"]
                                                                ++ alt
                                                                ++ [toHtml $ "</figcaption></figure>"]
                                                        where
                                                                (direct, css) = classToPlain att
--no description of video
media (Image ("video", att, att') [] (filename,_))  = return $ [toHtml $ "<video " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString ("",css,att') <> "></video>"]
                                                        where
                                                                (direct, css) = classToPlain att
--with description of video
media (Image ("video", att, att') alt (filename,_)) = return $ [toHtml $ "<figure>"]
                                                                ++ [toHtml $ "<video " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString ("",css,att') <> "></video>"]
                                                                ++ [toHtml $ "<figcaption>"]
                                                                ++ alt
                                                                ++ [toHtml $ "</figcaption></figure>"]
                                                        where
                                                                (direct, css) = classToPlain att
--no description of image
media (Image ("img", att, att') [] (filename,_))  = return $ [toHtml $ "<figure>"]
                                                              ++ [toHtml $ "<img " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString ("",css,att') <> "></img>"]
                                                              ++ [toHtml $ "</figure>"]
                                                        where
                                                                (direct, css) = classToPlain att
--with description of image
media (Image ("img", att, att') alt (filename,_)) = return $ [toHtml $ "<figure>"]
                                                                ++ [toHtml $ "<img " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString ("",css,att') <> "></img>"]
                                                                ++ [toHtml $ "<figcaption>"]
                                                                ++ alt
                                                                ++ [toHtml $ "</figcaption></figure>"]
                                                        where
                                                                (direct, css) = classToPlain att
media (Image ("svg", att, att') [] (filename,_))  = handle (\(fileerror :: IOException) -> return [toHtml $ "Could not read file: " <> filename <> "<br />" <> show fileerror]) $
                                                    do
                                                        svg <- readFile filename
                                                        return $ [toHtml $ "<figure " <> unwords direct <> " " <> attToString ("", css, att') <> ">"] -- use attributes on figure, as svg gets dumped in..
                                                              ++ [toHtml $ svg]
                                                              ++ [toHtml $ "</figure>"]
                                                        where
                                                                (direct, css) = classToPlain att
media (Image ("svg", att, att') alt (filename,_))  = handle (\(fileerror :: IOException) -> return $ [toHtml $ "Could not read file: " <> filename <> "<br />" <> show filename]) $
                                                     do
                                                        svg <- readFile filename
                                                        return $ [toHtml $ "<figure " <> unwords direct <> " " <> attToString ("", css, att') <> ">"] -- use attributes on figure, as svg gets dumped in..
                                                              ++ [toHtml $ svg]
                                                              ++ [toHtml $ "<figcaption>"]
                                                              ++ alt
                                                              ++ [toHtml $ "</figcaption></figure>"]
                                                        where
                                                                (direct, css) = classToPlain att
media (Image ("demo", att, att') [] (filename,_))  = return $ [toHtml $ "<iframe " <> unwords direct <> " src=\"" <> filename <> "?plugin\"" <> attToString ("", css, att') <> "></iframe>"]
                                                        where
                                                                (direct, css) = classToPlain att
media (Image ("demo", att, att') alt (filename,_))  = return $ [toHtml $ "<figure>"]
                                                              ++ [toHtml $ "<iframe " <> unwords direct <> " src=\"" <> filename <> "?plugin\"" <> attToString ("", css, att') <> "></iframe>"]
                                                              ++ [toHtml $ "<figcaption>"]
                                                              ++ alt
                                                              ++ [toHtml $ "</figcaption></figure>"]
                                                        where
                                                                (direct, css) = classToPlain att

media x = return [x]


attToString :: Attr -> String
attToString (ident, classes, kvpairs) = ident <> " class=\"" <> unwords classes <> "\" " <> unwords ((\(k,v) -> k <> "=\"" <> v <> "\"") <$> kvpairs)

classToPlain :: [String] -> ([String],[String])
classToPlain = partition (`elem` [ "data-autoplay"
                                 , "controls"
                                 ]
                         )

toHtml :: String -> Inline
toHtml = RawInline (Format "html")
