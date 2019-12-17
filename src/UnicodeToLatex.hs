module UnicodeToLatex where

import Data.Char (isAlphaNum)
import Data.Map.Strict ((!?), findWithDefault)
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
unicodeToLatex :: String -> String
unicodeToLatex = foldr f ""
  where
    f :: Char -> String -> String
    f x "" = findWithDefault [x] x unicodeToLatexMap
    f x (y:ys) =
        case unicodeToLatexMap !? x of
            Just x' -> x' ++ isolate y ++ ys
            Nothing -> x : y : ys
    isolate :: Char -> String
    isolate x
        | isAlphaNum x = ' ' : [x]
        | otherwise = [x]
