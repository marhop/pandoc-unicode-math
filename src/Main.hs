module Main where

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter unicodeToLatex

unicodeToLatex :: Inline -> Inline
unicodeToLatex (Math t e) = Math t (concatMap symbols e)
unicodeToLatex x          = x

symbols :: Char -> String
symbols '∧' = "\\land"
symbols '∨' = "\\lor"
symbols 'λ' = "\\lambda"
symbols '∀' = "\\forall"
symbols  s  = [s]
