{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Util.Filter.Media
    (media)
        where

import Text.Pandoc.JSON
import Control.Exception
import Data.Monoid ((<>))
import Data.Char (toLower)
import System.FilePath

import Text.Pandoc.Util.Filter

{-# ANN module "HLint: ignore Redundant $" #-} -- supress HLint-Warnings about $

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
  , "svg"
  ]

-- | File-extensions that should be treated as demo and will be included
-- in an iframe
demoExt :: [String]
demoExt = ["html", "htm"]

-- | main media-plugin.
--
-- Will convert the following syntax
--
-- @
-- ![](foo.aac){#audio}
-- ![](foo.mp4){#video}
-- ![](foo.png){#img}
-- ![](foo.svg){#svg}
-- ![](foo.html){#demo}
-- @
--
-- HTML-id's maybe ommitted if the file-extension is in whitelist.
--
-- If a type is detected by extension a custom id (not matching the extension)
-- will be preserved.
--
media :: Inline -> IO [Inline]
--audio
media (Image (id',att,att') [] (filename,_))
  | id' == "audio" || checkExtension filename audioExt
    = return $ [toHtml $ "<audio " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "audio" id',css,att') <> "></audio>"]
      where
        (direct, css) = classToRevealAttr att
media (Image (id',att,att') alt (filename,_))
  | id' == "audio" || checkExtension filename audioExt
    = return $ [toHtml $ "<figure><audio " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "audio" id',css,att') <> "></audio>"]
            <> [toHtml $ "<figcaption>"]
            <> alt
            <> [toHtml $ "</figcaption></figure>"]
      where
        (direct, css) = classToRevealAttr att
--videos
media (Image (id', att, att') [] (filename,_))
  | id' == "video" || checkExtension filename videoExt
    = return $ [toHtml $ "<video " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "video" id',css,att') <> "></video>"]
      where
        (direct, css) = classToRevealAttr att
media (Image (id', att, att') alt (filename,_))
  | id' == "video" || checkExtension filename videoExt
     = return $ [toHtml $ "<figure>"]
             <> [toHtml $ "<video " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "video" id',css,att') <> "></video>"]
             <> [toHtml $ "<figcaption>"]
             <> alt
             <> [toHtml $ "</figcaption></figure>"]
       where
         (direct, css) = classToRevealAttr att
--images
media (Image (id', att, att') [] (filename,_))
  | id' == "img" || checkExtension filename imgExt
    = return $ [toHtml $ "<figure>"]
            <> [toHtml $ "<img " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "img" id',css,att') <> "></img>"]
            <> [toHtml $ "</figure>"]
      where
        (direct, css) = classToRevealAttr att
media (Image (id', att, att') alt (filename,_))
  | id' == "img" || checkExtension filename imgExt
    = return $ [toHtml $ "<figure>"]
            <> [toHtml $ "<img " <> unwords direct <> " src=\"" <> filename <> "\"" <> attToString (idFilter "img" id',css,att') <> "></img>"]
            <> [toHtml $ "<figcaption>"]
            <> alt
            <> [toHtml $ "</figcaption></figure>"]
      where
        (direct, css) = classToRevealAttr att
--load svg and dump it in
media (Image (id', att, att') [] (filename,_))
  | id' == "svg"
    = handle (\(fileerror :: IOException) -> return [toHtml $ "Could not read file: " <> filename <> "<br />" <> show fileerror]) $
                do
                  svg <- readFile filename
                  return $ [toHtml $ "<figure " <> unwords direct <> " " <> attToString (idFilter "svg" id', css, att') <> ">"] -- use attributes on figure, as svg gets dumped in..
                        <> [toHtml $ svg]
                        <> [toHtml $ "</figure>"]
      where
        (direct, css) = classToRevealAttr att
media (Image (id', att, att') alt (filename,_))
  | id' == "svg"
    = handle (\(fileerror :: IOException) -> return [toHtml $ "Could not read file: " <> filename <> "<br />" <> show fileerror]) $
                do
                  svg <- readFile filename
                  return $ [toHtml $ "<figure " <> unwords direct <> " " <> attToString (idFilter "svg" id', css, att') <> ">"] -- use attributes on figure, as svg gets dumped in..
                        <> [toHtml $ svg]
                        <> [toHtml $ "<figcaption>"]
                        <> alt
                        <> [toHtml $ "</figcaption></figure>"]
      where
        (direct, css) = classToRevealAttr att
--html-demos etc. as IFrames
media (Image (id', att, att') [] (filename,_))
  | id' == "demo" || checkExtension filename demoExt
    = return [toHtml $ "<iframe " <> unwords direct <> " src=\"" <> filename <> "?plugin\"" <> attToString (idFilter "demo" id', css, att') <> "></iframe>"]
      where
        (direct, css) = classToRevealAttr att
media (Image (id', att, att') alt (filename,_))
  | id' == "demo" || checkExtension filename demoExt
    = return $ [toHtml $ "<figure>"]
            <> [toHtml $ "<iframe " <> unwords direct <> " src=\"" <> filename <> "?plugin\"" <> attToString (idFilter "demo" id', css, att') <> "></iframe>"]
            <> [toHtml $ "<figcaption>"]
            <> alt
            <> [toHtml $ "</figcaption></figure>"]
      where
        (direct, css) = classToRevealAttr att
-- if not matched
media x = return [x]

checkExtension :: String -> [String] -> Bool
checkExtension fn exts = (fmap toLower . tail . takeExtension) fn `elem` exts

idFilter :: String -> String -> String
idFilter a b
  | a == b    = ""
  | otherwise = b

