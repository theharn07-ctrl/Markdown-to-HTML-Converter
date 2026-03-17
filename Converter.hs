module Converter where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)

-- Block-level: processes lines → HTML blocks
convertLine :: String -> String
convertLine line
  | "### " `isPrefixOf` line = tag "h3" (drop 4 line)
  | "## "  `isPrefixOf` line = tag "h2" (drop 3 line)
  | "# "   `isPrefixOf` line = tag "h1" (drop 2 line)
  | "- "   `isPrefixOf` line = tag "li" (drop 2 line)
  | null line                 = "<br/>"
  | otherwise                 = tag "p"  line
  where
    tag t content = "<" ++ t ++ ">" ++ inlineConvert content ++ "</" ++ t ++ ">"

-- Inline-level: handles bold, italic, code within a line
inlineConvert :: String -> String
inlineConvert [] = []
inlineConvert ('*':'*':rest) =
  let (inner, after) = break (== '*') rest  -- simplistic, good enough to start
  in "<strong>" ++ inner ++ "</strong>" ++ inlineConvert (drop 2 after)
inlineConvert ('*':rest) =
  let (inner, after) = break (== '*') rest
  in "<em>" ++ inner ++ "</em>" ++ inlineConvert after
inlineConvert ('`':rest) =
  let (inner, after) = break (== '`') rest
  in "<code>" ++ inner ++ "</code>" ++ inlineConvert after
inlineConvert (c:rest) = c : inlineConvert rest

-- Wrap everything in a valid HTML document
wrapHTML :: String -> String
wrapHTML body = unlines
  [ "<!DOCTYPE html>"
  , "<html><head><meta charset='utf-8'></head><body>"
  , body
  , "</body></html>"
  ]

convert :: String -> String
convert = wrapHTML . unlines . map convertLine . lines