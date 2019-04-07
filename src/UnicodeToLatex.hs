module UnicodeToLatex where

import Data.Map.Strict (findWithDefault)
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
unicodeToLatex :: String -> String
unicodeToLatex = concatMap (\x -> findWithDefault [x] x unicodeToLatexMap)
