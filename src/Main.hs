module Main where

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter unicodeToLatex

unicodeToLatex :: Inline -> [Inline]
unicodeToLatex (Math t e) = [Math t (concatMap sym e)]
unicodeToLatex (Str s) = foldr merge [] $ map (wrap sym) s
unicodeToLatex x = [x]

wrap :: (Char -> String) -> Char -> Inline
wrap f c =
    let c' = f c
    in if [c] == c'
           then Str c'
           else Math InlineMath c'

merge :: Inline -> [Inline] -> [Inline]
merge (Str s1) ((Str s2):xs) = Str (s1 ++ s2) : xs
merge (Math InlineMath e1) ((Math InlineMath e2):xs) =
    Math InlineMath (e1 ++ e2) : xs
merge x xs = x : xs

sym :: Char -> String
sym '¬' = "\\neg"
sym '±' = "\\pm"
sym '×' = "\\times"
sym '÷' = "\\div"
sym '…' = "\\dots"
sym 'ℕ' = "\\mathbb{N}"
sym 'ℚ' = "\\mathbb{Q}"
sym 'ℝ' = "\\mathbb{R}"
sym 'ℤ' = "\\mathbb{Z}"
sym '←' = "\\leftarrow"
sym '↑' = "\\uparrow"
sym '→' = "\\rightarrow"
sym '↓' = "\\downarrow"
sym '↔' = "\\leftrightarrow"
sym '⇒' = "\\Rightarrow"
sym '⇔' = "\\Leftrightarrow"
sym '∀' = "\\forall"
sym '∃' = "\\exists"
sym '∅' = "\\emptyset"
sym '∈' = "\\in"
sym '∉' = "\\notin"
sym '∋' = "\\ni"
sym '∎' = "\\blacksquare"
sym '∑' = "\\sum"
sym '∓' = "\\mp"
sym '∗' = "\\ast"
sym '∘' = "\\circ"
sym '∙' = "\\bullet"
sym '∝' = "\\propto"
sym '∞' = "\\infty"
sym '∥' = "\\parallel"
sym '∧' = "\\land"
sym '∨' = "\\lor"
sym '∩' = "\\cap"
sym '∪' = "\\cup"
sym '∴' = "\\therefore"
sym '∵' = "\\because"
sym '≠' = "\\neq"
sym '≡' = "\\equiv"
sym '≤' = "\\leq"
sym '≥' = "\\geq"
sym '⊂' = "\\subset"
sym '⊃' = "\\supset"
sym '⊆' = "\\subseteq"
sym '⊇' = "\\supseteq"
sym '⊢' = "\\vdash"
sym '⊤' = "\\top"
sym '⊥' = "\\bot"
sym '⊨' = "\\vDash"
sym '⋅' = "\\cdot"
sym '⋮' = "\\vdots"
sym '⋯' = "\\cdots"
sym 'α' = "\\alpha"
sym 'Α' = "A"
sym 'β' = "\\beta"
sym 'Β' = "B"
sym 'γ' = "\\gamma"
sym 'Γ' = "\\Gamma"
sym 'δ' = "\\delta"
sym 'Δ' = "\\Delta"
sym 'ε' = "\\varepsilon"
sym 'ϵ' = "\\epsilon"
sym 'Ε' = "E"
sym 'ζ' = "\\zeta"
sym 'Ζ' = "Z"
sym 'η' = "\\eta"
sym 'Η' = "H"
sym 'θ' = "\\theta"
sym 'ϑ' = "\\vartheta"
sym 'Θ' = "\\Theta"
sym 'ι' = "\\iota"
sym 'Ι' = "I"
sym 'κ' = "\\kappa"
sym 'Κ' = "K"
sym 'λ' = "\\lambda"
sym 'Λ' = "\\Lambda"
sym 'μ' = "\\mu"
sym 'Μ' = "M"
sym 'ν' = "\\nu"
sym 'Ν' = "N"
sym 'ξ' = "\\xi"
sym 'Ξ' = "\\Xi"
sym 'ο' = "o"
sym 'Ο' = "O"
sym 'π' = "\\pi"
sym 'Π' = "\\Pi"
sym 'ρ' = "\\rho"
sym 'ϱ' = "\\varrho"
sym 'Ρ' = "P"
sym 'σ' = "\\sigma"
sym 'ς' = "\\varsigma"
sym 'Σ' = "\\Sigma"
sym 'τ' = "\\tau"
sym 'Τ' = "T"
sym 'υ' = "\\upsilon"
sym 'Υ' = "\\Upsilon"
sym 'φ' = "\\varphi"
sym 'ϕ' = "\\phi"
sym 'Φ' = "\\Phi"
sym 'χ' = "\\chi"
sym 'Χ' = "X"
sym 'ψ' = "\\psi"
sym 'Ψ' = "\\Psi"
sym 'ω' = "\\omega"
sym 'Ω' = "\\Omega"
sym s = [s]
