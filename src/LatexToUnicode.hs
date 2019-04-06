module LatexToUnicode where

import PandocUnicodeMath
import Text.Pandoc.JSON (toJSONFilter)

main :: IO ()
main = toJSONFilter (unicodeMath LatexToUnicode)
