{-# LANGUAGE OverloadedStrings #-}

module LatexToUnicode where

import Prelude hiding (lookup)

import Data.Map.Strict (lookup)
import Data.Text (Text, pack)
import Text.LaTeX.Base.Parser (parseLaTeX)
import Text.LaTeX.Base.Render (render)
import Text.LaTeX.Base.Syntax (LaTeX (..), TeXArg (FixArg))
import Text.Pandoc.JSON (toJSONFilter)

import MathFilter
import Symbols

main :: IO ()
main = toJSONFilter (mathFilter latexToUnicode)

-- | Replace Latex math commands by equivalent Unicode symbols. Examples:
--
--   * \alpha → α
--   * \mathbb{N} → ℕ
--   * but /not/ A → Α (latin A to greek Alpha) because that's ambiguous
latexToUnicode :: Text -> Text
latexToUnicode = either (error . show) (render . go) . parseLaTeX
  where
    go :: LaTeX -> LaTeX
    -- e.g. \alpha
    go orig@(TeXCommS x) =
      maybe orig TeXRaw $ lookup ("\\" <> pack x) latexToUnicodeMap
    -- e.g. \mathbb{N}
    go orig@(TeXComm x [FixArg (TeXRaw y)]) =
      maybe orig TeXRaw $
        lookup ("\\" <> pack x <> "{" <> y <> "}") latexToUnicodeMap
    -- nested Latex expressions
    go (TeXEnv x ys z) = TeXEnv x ys (go z)
    go (TeXMath x y) = TeXMath x (go y)
    go (TeXBraces x) = TeXBraces (go x)
    go (TeXSeq x y) = TeXSeq (go x) (go y)
    go x = x
