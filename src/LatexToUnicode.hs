{-# LANGUAGE QuasiQuotes #-}

module LatexToUnicode where

import Data.Map.Strict (findWithDefault)
import Text.Pandoc.JSON (toJSONFilter)
import Text.Regex.PCRE.Heavy (Regex, gsub, re)

import MathFilter
import Symbols

main :: IO ()
main = toJSONFilter (mathFilter latexToUnicode)

-- | Replace Latex math commands by equivalent Unicode symbols. Examples:
--
--   * \alpha → α
--   * \mathbb{N} → ℕ
--   * but /not/ A → Α (latin A to greek Alpha) because that's ambiguous
latexToUnicode :: String -> String
latexToUnicode = gsub latexCommand (\x -> findWithDefault x x latexToUnicodeMap)

-- | Regex describing a latex command like "\alpha" or "\mathbb{N}".
latexCommand :: Regex
latexCommand = [re|\\\w+(?:\{\w+\})?|]
