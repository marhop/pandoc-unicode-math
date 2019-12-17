# Release 1.2.0

2019-12-17

The `pandoc-unicode-math` filter now adds sensible whitespace where necessary:
The Unicode sequence `λx` now translates to `\lambda x` (note the added
whitespace) because the previous `\lambdax` would raise a Latex error, but `αβ`
still translates to `\alpha\beta` because this is unambiguous. Idea by Fynn
Leitow.

Several new symbols were added. Contributed by Fynn Leitow.

# Release 1.1.0

2019-04-09

A second filter `pandoc-unicode-math-from-latex` was added that replaces
"regular" Latex math commands by equivalent Unicode symbols. This is more or
less the inverse of the existing filter `pandoc-unicode-math`. Idea by Anish
Mittal.

Several new symbols were added. Partly contributed by Eric Hanson.

# Release 1.0.0

2018-02-27

Initial release.
