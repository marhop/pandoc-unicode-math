# pandoc-unicode-math

[![CI](https://github.com/marhop/pandoc-unicode-math/actions/workflows/ci.yml/badge.svg)](https://github.com/marhop/pandoc-unicode-math/actions/workflows/ci.yml)

If you prefer Markdown to Latex because of its readability, then why not
improve the readability of math expressions as well? Just use the power of
Unicode!

## What does this filter do?

This [Pandoc] [filter] replaces Unicode math symbols and greek letters like ∀,
∈, →, λ, or Ω in math environments by equivalent Latex commands like
`\forall`, `\in`, `\rightarrow`, `\lambda`, or `\Omega`. That means you can
write expressions like these in Markdown and still get Latex's superior math
typesetting when converting to PDF:

    α → β ≡ ¬α ∨ β ≡ ¬(α ∧ ¬β)

    ∀ x ∈ \{ 2, 4, … \}, ∀ y ∈ ℕ : (x ⋅ y) mod 2 = 0

    ℕ ⊂ ℤ ⊂ ℝ

Now compare them to the same expressions written with Latex commands:

    \alpha \rightarrow \beta \equiv \neg \alpha \lor \beta \equiv \neg (\alpha \land \neg \beta)

    \forall x \in \{ 2, 4, \dots \}, \forall y \in \mathbb{N} : (x \cdot y) mod 2 = 0

    \mathbb{N} \subset \mathbb{Z} \subset \mathbb{R}

If you prefer the Unicode variant, this filter is for you!

A complete list of symbols that are replaced by this filter can be found in the
[`src/Symbols.hs`](src/Symbols.hs) file.

[Pandoc]: https://pandoc.org/
[filter]: https://pandoc.org/filters.html

## Usage

 1. Get binaries [here][releases] and put them in your [PATH]. Be careful to
    choose a release that matches your Pandoc version (otherwise you'll get an
    error like `Incompatible API versions: encoded with [1,20] but attempted to
    decode with [1,21].`). If necessary, you can build from source (see below).
 2. Write a Markdown document containing Unicode characters like the provided
    [example file](example.md).
 3. Invoke Pandoc to convert the Markdown document to PDF and apply the filter
    along the way:

        $ pandoc example.md --filter pandoc-unicode-math -o example.pdf

[releases]: https://github.com/marhop/pandoc-unicode-math/releases
[PATH]: https://en.wikipedia.org/wiki/PATH_(variable)

## But my keyboard has no "α" and "∃" keys!

The `pandoc-unicode-math` filter will not make *writing* math expressions
easier. It only makes *reading* them easier. How to input Unicode symbols
depends on your editor. In Vim, [digraphs] and the [characterize plugin] are
very useful for working with Unicode.

However, to help you get started with existing documents a second filter called
`pandoc-unicode-math-from-latex` is provided that replaces "regular" Latex math
commands like `\forall` or `\alpha` by equivalent Unicode symbols like ∀ or α.
Yes, that means it's the inverse of the `pandoc-unicode-math` filter, except
that `pandoc-unicode-math` replaces characters like Α (greek Alpha) by A (latin
A) but `pandoc-unicode-math-from-latex` does not do the reverse since this would
be ambiguous. This filter can be used like this:

    $ pandoc latex-math.md --filter pandoc-unicode-math-from-latex -o unicode-math.md

[digraphs]: http://vimdoc.sourceforge.net/htmldoc/digraph.html
[characterize plugin]: https://github.com/tpope/vim-characterize

## Limitations

For more complex math containing fractions like `\frac{2}{3+5}` or lots of
subscripts and superscripts like `k_{n+1}^2` you still need raw Latex because
these things cannot be expressed by single Unicode characters. However,
Unicode symbols and Latex commands can be used together in the same
expression:

    ∃ x ∈ ℕ : \frac{x}{2} = 21

## Alternatives

Instead of writing Unicode symbols in your source file and applying this filter,
you can write regular Latex math commands and use an editor that displays them
in a more readable way. For example, using Vim and the [Markdown plugin], these
settings have the desired effect:

    let g:vim_markdown_math=1
    set conceallevel=2

[Markdown plugin]: https://github.com/plasticboy/vim-markdown

## Building from source

These filters are written in Haskell, so you need GHC (compiler) and Cabal
(build tool), best installed with [ghcup] if you use a Unix-like OS. You also
need the [pkg-config] tool and the [PCRE] library (these are required by one of
the used Haskell libraries, namely the pcre-heavy package).

Clone the Git repository, change to its top level directory and run the
following command:

    $ cabal install --constraint 'pandoc-types==1.22'

On Linux, this will build and install two filters, `pandoc-unicode-math` and
`pandoc-unicode-math-from-latex` to `~/.cabal/bin/` and on Windows, well, I
don't know but surely somewhere sensible.

In the above command you have to choose a version of the pandoc-types library
that matches your Pandoc release. (If you run `cabal build` in a separate step
the `--constraint` option should be included there as well.) For reference, here
is a compatibility list:

pandoc-types | pandoc
-------------|---------
1.22         | ≥ 2.11
1.21         | 2.10
1.20         | 2.8-2.9

[ghcup]: https://www.haskell.org/ghcup/
[pkg-config]: https://www.freedesktop.org/wiki/Software/pkg-config/
[PCRE]: https://pcre.org/

## Contributing

Pull Requests are welcome. It's easy to add new symbols to the
[`src/Symbols.hs`](src/Symbols.hs) file.
