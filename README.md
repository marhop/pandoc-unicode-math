# pandoc-unicode-math

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
[`src/Main.hs`](src/Main.hs) file.

## Usage

 1. Get a binary [here][releases] and put it in your [PATH]. If necessary, you
    can instead build from source (see below).
 2. Write a Markdown document containing Unicode characters like the provided
    [example file](example.md).
 3. Invoke Pandoc to convert the Markdown document to PDF and apply the filter
    along the way:

        $ pandoc example.md --filter pandoc-unicode-math -o example.pdf

## But my keyboard has no "α" and "∃" keys!

This filter will not make *writing* math expressions easier. It only makes
*reading* them easier. How to input Unicode symbols depends on your editor. In
Vim, [digraphs] and the [characterize plugin] are very useful for working with
Unicode.

## Limitations

For more complex math containing fractions like `\frac{2}{3+5}` or lots of
subscripts and superscripts like `k_{n+1}^2` you still need raw Latex because
these things cannot be expressed by single Unicode characters. However,
Unicode symbols and Latex commands can be used together in the same
expression:

    ∃ x ∈ ℕ : \frac{x}{2} = 21

## Building from source

This filter is written in Haskell. It is recommended to use [Stack] for
building. Install Stack, the [pkg-config] tool and the [PCRE] library (these are
required by one of the used Haskell libraries, namely the pcre-heavy package).
On Debian:

    # apt install haskell-stack pkg-config libpcre++-dev

Then clone the Git repository, change to its top level directory and run the
following commands:

    $ stack setup
    $ stack build
    $ stack install

On Linux, this will install the `pandoc-unicode-math` filter to `~/.local/bin/`
and on Windows, well, I don't know but surely somewhere sensible.

Maybe you would like to adapt the [`stack.yaml`](stack.yaml) file prior to
building to select a [Stackage] snapshot that fits your environment. For
example, use `resolver: lts-7.24` to create a binary that is compatible with
Pandoc 1.17 (shipped with Debian 9) or `resolver: lts-9.21` for Pandoc 1.19 and
later, including Pandoc 2 (there was an API change between 1.17 and 1.19, so you
need different binaries).

[Pandoc]: https://pandoc.org/
[filter]: https://pandoc.org/filters.html
[releases]: https://github.com/marhop/pandoc-unicode-math/releases
[PATH]: https://en.wikipedia.org/wiki/PATH_(variable)
[digraphs]: http://vimdoc.sourceforge.net/htmldoc/digraph.html
[characterize plugin]: https://github.com/tpope/vim-characterize
[Stack]: https://docs.haskellstack.org/
[Stackage]: https://www.stackage.org/
[pkg-config]: https://www.freedesktop.org/wiki/Software/pkg-config/
[PCRE]: https://pcre.org/
