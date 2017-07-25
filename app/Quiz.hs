#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Control.Exception
import Data.Monoid ((<>))
import Data.List (partition)

main :: IO ()
main = toJSONFilter quizLift

quizLift :: Block -> [Block]
quizLift pb@(Plain b) = fmap makeQuiz (query findQuiz pb) ++ [Plain (filter ((==) [] . findQuiz) b)]
quizLift pb@(Para  b) = fmap makeQuiz (query findQuiz pb) ++ [Plain (filter ((==) [] . findQuiz) b)]
quizLift x = [x]


findQuiz :: Inline -> [(Attr, [Inline], Maybe [Inline])]
findQuiz (Span attributes@(id, att, att') answerText)
  | "answer" `elem` att     = [(attributes, answerText, Nothing)]
findQuiz (Link attributes@(id, att, att') answerText (tooltip,_))
  | "answer" `elem` att     = [(attributes, answerText, Just [toHtml tooltip])]
findQuiz x = []

makeQuiz :: (Attr, [Inline], Maybe [Inline]) -> Block
makeQuiz (att, answer, Nothing) = Div att [Plain answer]
makeQuiz (att, answer, Just tooltip) = Div att [Plain answer, Div ("",["tooltip"],[]) [Plain tooltip]]

-- quiz :: Inline -> [Inline]
-- quiz (Span attributes@(id, att, att') answerText)
--   | "answer" `elem` att     = [toHtml $ "<div " <> attToString attributes <> ">"]
--                               ++ answerText
--                               ++ [toHtml $ "</div>"]
-- quiz (Link attributes@(id, att, att') answerText (tooltip,_))
--   | "answer" `elem` att     = [toHtml $ "<div " <> attToString attributes <> ">"]
--                               ++ answerText
--                               ++ [toHtml $ "<div class=\"tooltip\">" <> tooltip <> "</div>"]
--                               ++ [toHtml $ "</div>"]
-- quiz x = [x]


attToString :: Attr -> String
attToString (ident, classes, kvpairs) = ident <> " class=\"" <> unwords classes <> "\" " <> unwords ((\(k,v) -> k <> "=\"" <> v <> "\"") <$> kvpairs)

toHtml :: String -> Inline
toHtml = RawInline (Format "html")
