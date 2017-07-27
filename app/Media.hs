#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Pandoc.JSON
import Control.Exception
import Data.Monoid ((<>))
import Data.List (partition, elem)
import System.FilePath

main :: IO ()
main = toJSONFilter media

-- | File-extensions that should be treated as audio
audioExt :: [String]
audioExt = ["mp3","aac"]

-- | File-extensions that should be treated as video
videoExt :: [String]
videoExt = [ "avi"
           , "mp4"
           , "mov"
           ]

-- | File-extensions that should be treated as image
imgExt :: [String]
imgExt = 
  [ "jpg"
  , "jpeg"
  , "png"
  , "gif"
  , "tif"
  , "tiff"
  , "bmp"
  ]

-- | File-extensions that should be treated as demo and will be included
-- in an iframe.
demoExt :: [String]
demoExt = ["html", "htm"]

-- | main media-plugin.
--
-- Will convert the following syntax
--
-- 
-- - `![](foo.aac){#audio}`
-- - `![](foo.mp4){#video}`
-- - `![](foo.png){#img}`
-- - `![](foo.svg){#svg}`
-- - `![](foo.html){#demo}`
--
-- HTML-id's maybe ommitted if the file-extension is in whitelist.
--
-- If a type is detected by extension a custom id (not matching the extension)
-- will be preserved.
--
media :: Inline -> IO [Inline]
--audio
media (Image (id',att,att') [] (filename,_))
  | id' == "audio" || (takeExtension filename `elem` audioExt)
    = return $ [toHtml $ "<audio " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "audio" id',css,att') <> "></audio>"]
      where
        (direct, css) = classToPlain att
media (Image (id',att,att') alt (filename,_))
  | id' == "audio" || (takeExtension filename `elem` audioExt)
    = return $ [toHtml $ "<figure><audio " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "audio" id',css,att') <> "></audio>"]
            ++ [toHtml $ "<figcaption>"]
            ++ alt
            ++ [toHtml $ "</figcaption></figure>"]
      where
        (direct, css) = classToPlain att
--videos
media (Image (id', att, att') [] (filename,_))
  | id' == "video" || (takeExtension filename `elem` videoExt)
    = return $ [toHtml $ "<video " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "video" id',css,att') <> "></video>"]
      where
        (direct, css) = classToPlain att
media (Image (id', att, att') alt (filename,_))
  | id' == "video" || (takeExtension filename `elem` videoExt)
     = return $ [toHtml $ "<figure>"]
             ++ [toHtml $ "<video " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "video" id',css,att') <> "></video>"]
             ++ [toHtml $ "<figcaption>"]
             ++ alt
             ++ [toHtml $ "</figcaption></figure>"]
       where
         (direct, css) = classToPlain att
--images
media (Image (id', att, att') [] (filename,_))
  | id' == "img" || (takeExtension filename `elem` imgExt)
    = return $ [toHtml $ "<figure>"]
            ++ [toHtml $ "<img " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "img" id',css,att') <> "></img>"]
            ++ [toHtml $ "</figure>"]
      where
        (direct, css) = classToPlain att
media (Image (id', att, att') alt (filename,_))
  | id' == "img" || (takeExtension filename `elem` imgExt)
    = return $ [toHtml $ "<figure>"]
            ++ [toHtml $ "<img " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "img" id',css,att') <> "></img>"]
            ++ [toHtml $ "<figcaption>"]
            ++ alt
            ++ [toHtml $ "</figcaption></figure>"]
      where
        (direct, css) = classToPlain att
--load svg and dump it in
media (Image (id', att, att') [] (filename,_))
  | id' == "svg" || (takeExtension filename == "svg")
    = handle (\(fileerror :: IOException) -> return [toHtml $ "Could not read file: " <> filename <> "<br />" <> show fileerror]) $
                do
                  svg <- readFile filename
                  return $ [toHtml $ "<figure " <> unwords direct <> " " <> attToString (idFilter "svg" id', css, att') <> ">"] -- use attributes on figure, as svg gets dumped in..
                        ++ [toHtml $ svg]
                        ++ [toHtml $ "</figure>"]
      where
        (direct, css) = classToPlain att
media (Image (id', att, att') alt (filename,_))
  | id' == "svg" || (takeExtension filename == "svg")
    = handle (\(fileerror :: IOException) -> return $ [toHtml $ "Could not read file: " <> filename <> "<br />" <> show filename]) $
                do
                  svg <- readFile filename
                  return $ [toHtml $ "<figure " <> unwords direct <> " " <> attToString (idFilter "svg" id', css, att') <> ">"] -- use attributes on figure, as svg gets dumped in..
                        ++ [toHtml $ svg]
                        ++ [toHtml $ "<figcaption>"]
                        ++ alt
                        ++ [toHtml $ "</figcaption></figure>"]
      where
        (direct, css) = classToPlain att
--html-demos etc. as IFrames
media (Image (id', att, att') [] (filename,_))
  | id' == "demo" || (takeExtension filename `elem` demoExt)
    = return $ [toHtml $ "<iframe " <> unwords direct <> " src=\"" <> filename <> "?plugin\"" <> attToString (idFilter "demo" id', css, att') <> "></iframe>"]
      where
        (direct, css) = classToPlain att
media (Image (id', att, att') alt (filename,_))
  | id' == "demo" || (takeExtension filename `elem` demoExt)
    = return $ [toHtml $ "<figure>"]
            ++ [toHtml $ "<iframe " <> unwords direct <> " src=\"" <> filename <> "?plugin\"" <> attToString (idFilter "demo" id', css, att') <> "></iframe>"]
            ++ [toHtml $ "<figcaption>"]
            ++ alt
            ++ [toHtml $ "</figcaption></figure>"]
      where
        (direct, css) = classToPlain att
-- if not matched
media x = return [x]


attToString :: Attr -> String
attToString ("", classes, kvpairs) = "class=\"" <> unwords classes <> "\" " <> unwords ((\(k,v) -> k <> "=\"" <> v <> "\"") <$> kvpairs)
attToString (id', classes, kvpairs) = "id=\"" <> id'  <> "\" class=\"" <> unwords classes <> "\" " <> unwords ((\(k,v) -> k <> "=\"" <> v <> "\"") <$> kvpairs)


idFilter :: String -> String -> String
idFilter a b
  | a == b    = ""
  | otherwise = a

classToPlain :: [String] -> ([String],[String])
classToPlain = partition (`elem` [ "data-autoplay"
                                 , "controls"
                                 ]
                         )

toHtml :: String -> Inline
toHtml = RawInline (Format "html")
