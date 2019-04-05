{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Map.Strict (Map, findWithDefault, fromList)
import Text.Pandoc.JSON (Inline(Math), toJSONFilter)
import Text.Regex.PCRE.Heavy (Regex, gsub, re)

main :: IO ()
main = toJSONFilter unicodeMath

unicodeMath :: Inline -> Inline
unicodeMath (Math t e) = Math t (unicodeToLatex e)
-- TODO Make available to user.
-- unicodeMath (Math t e) = Math t (latexToUnicode e)
unicodeMath x = x

-- | Replace Unicode math symbols in a string by equivalent Latex commands.
-- Examples:
--
--   * α → \alpha
--   * ℕ → \mathbb{N}
--   * Α → A (greek Alpha to latin A), ugly but that's how Latex handles it
unicodeToLatex :: String -> String
unicodeToLatex = concatMap (\x -> findWithDefault [x] x unicodeToLatexMap)

-- | Map from Unicode symbols to Latex commands.
unicodeToLatexMap :: Map Char String
unicodeToLatexMap = fromList symbols

-- | Replace Latex math commands by equivalent Unicode symbols. Examples:
--
--   * \alpha → α
--   * \mathbb{N} → ℕ
--   * but /not/ A → Α (latin A to greek Alpha) because that's ambiguous
latexToUnicode :: String -> String
latexToUnicode = gsub latexCommand (\x -> findWithDefault x x latexToUnicodeMap)

-- | Map from Latex commands to Unicode symbols.
latexToUnicodeMap :: Map String String
latexToUnicodeMap = fromList [(y, [x]) | (x, y) <- symbols]

-- | Regex describing a latex command like "\alpha" or "\mathbb{N}".
latexCommand :: Regex
latexCommand = [re|\\\w+(?:\{\w+\})?|]

symbols :: [(Char, String)]
symbols =
    [ ('¬', "\\neg")
    , ('±', "\\pm")
    , ('×', "\\times")
    , ('÷', "\\div")
    , ('…', "\\dots")
    , ('ℕ', "\\mathbb{N}")
    , ('ℚ', "\\mathbb{Q}")
    , ('ℝ', "\\mathbb{R}")
    , ('ℤ', "\\mathbb{Z}")
    , ('←', "\\leftarrow")
    , ('↑', "\\uparrow")
    , ('→', "\\rightarrow")
    , ('↓', "\\downarrow")
    , ('↔', "\\leftrightarrow")
    , ('⇒', "\\Rightarrow")
    , ('⇔', "\\Leftrightarrow")
    , ('∀', "\\forall")
    , ('∃', "\\exists")
    , ('∅', "\\emptyset")
    , ('∈', "\\in")
    , ('∉', "\\notin")
    , ('∋', "\\ni")
    , ('∎', "\\blacksquare")
    , ('∑', "\\sum")
    , ('∓', "\\mp")
    , ('∗', "\\ast")
    , ('∘', "\\circ")
    , ('∙', "\\bullet")
    , ('∝', "\\propto")
    , ('∞', "\\infty")
    , ('∥', "\\parallel")
    , ('∧', "\\land")
    , ('∨', "\\lor")
    , ('∩', "\\cap")
    , ('∪', "\\cup")
    , ('∴', "\\therefore")
    , ('∵', "\\because")
    , ('≈', "\\approx")
    , ('≠', "\\neq")
    , ('≡', "\\equiv")
    , ('≤', "\\leq")
    , ('≥', "\\geq")
    , ('⊂', "\\subset")
    , ('⊃', "\\supset")
    , ('⊆', "\\subseteq")
    , ('⊇', "\\supseteq")
    , ('⊢', "\\vdash")
    , ('⊤', "\\top")
    , ('⊥', "\\bot")
    , ('⊨', "\\vDash")
    , ('⋅', "\\cdot")
    , ('⋮', "\\vdots")
    , ('⋯', "\\cdots")
    , ('α', "\\alpha")
    , ('Α', "A")
    , ('β', "\\beta")
    , ('Β', "B")
    , ('γ', "\\gamma")
    , ('Γ', "\\Gamma")
    , ('δ', "\\delta")
    , ('Δ', "\\Delta")
    , ('ε', "\\varepsilon")
    , ('ϵ', "\\epsilon")
    , ('Ε', "E")
    , ('ζ', "\\zeta")
    , ('Ζ', "Z")
    , ('η', "\\eta")
    , ('Η', "H")
    , ('θ', "\\theta")
    , ('ϑ', "\\vartheta")
    , ('Θ', "\\Theta")
    , ('ι', "\\iota")
    , ('Ι', "I")
    , ('κ', "\\kappa")
    , ('Κ', "K")
    , ('λ', "\\lambda")
    , ('Λ', "\\Lambda")
    , ('μ', "\\mu")
    , ('Μ', "M")
    , ('∇', "\\nabla")
    , ('ν', "\\nu")
    , ('Ν', "N")
    , ('ξ', "\\xi")
    , ('Ξ', "\\Xi")
    , ('ο', "o")
    , ('Ο', "O")
    , ('π', "\\pi")
    , ('Π', "\\Pi")
    , ('ρ', "\\rho")
    , ('ϱ', "\\varrho")
    , ('Ρ', "P")
    , ('σ', "\\sigma")
    , ('ς', "\\varsigma")
    , ('Σ', "\\Sigma")
    , ('τ', "\\tau")
    , ('Τ', "T")
    , ('υ', "\\upsilon")
    , ('Υ', "\\Upsilon")
    , ('φ', "\\varphi")
    , ('ϕ', "\\phi")
    , ('Φ', "\\Phi")
    , ('χ', "\\chi")
    , ('Χ', "X")
    , ('ψ', "\\psi")
    , ('Ψ', "\\Psi")
    , ('ω', "\\omega")
    , ('Ω', "\\Omega")
    ]
