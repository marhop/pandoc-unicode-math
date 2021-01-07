{-# LANGUAGE OverloadedStrings #-}

module UnicodeToLatex where

import Data.Char (isAlphaNum)
import Data.Map.Strict (findWithDefault, (!?))
import Data.Text (Text, cons, singleton, snoc, uncons)
import qualified Data.Text as T
import Text.Pandoc.JSON (toJSONFilter)

import MathFilter
import Symbols

main :: IO ()
main = toJSONFilter (mathFilter unicodeToLatex)

-- | Replace Unicode math symbols in a string by equivalent Latex commands.
-- Examples:
--
--   * α → \alpha
--   * ℕ → \mathbb{N}
--   * Α → A (greek Alpha to latin A), ugly but that's how Latex handles it
--
-- Sensible whitespace is added where necessary:
--
--   * λx → \lambda x
--   * αβ → \alpha\beta
unicodeToLatex :: Text -> Text
unicodeToLatex = T.foldr f ""
  where
    f :: Char -> Text -> Text
    f x acc
      | Just (y, ys) <- uncons acc =
        maybe (x `cons` acc) (<> isolate y <> ys) (unicodeToLatexMap !? x)
      | otherwise = findWithDefault (singleton x) x unicodeToLatexMap
    isolate :: Char -> Text
    isolate x
      | isAlphaNum x = " " `snoc` x
      | otherwise = singleton x
