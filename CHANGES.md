# Release 3.0.1

2021-01-09

This release can be built with a broader range of GHC versions (at least 8.6,
8.8, 8.10 instead of just 8.8 with release 2.0.0). No functional changes.

Pandoc compatibility: 2.10

# Release 3.0.0

2021-01-08

Pandoc compatibility: 2.10

# Release 2.0.1

2021-01-09

This release can be built with a broader range of GHC versions (at least 8.6,
8.8, 8.10 instead of just 8.8 with release 2.0.0). No functional changes.

Pandoc compatibility: 2.8-2.9

# Release 2.0.0

2021-01-08

Starting with this release, version number increments will also reflect
compatibility with Pandoc versions i.e., if a new release of this filter is not
compatible with an older Pandoc version anymore this will lead to a major
version increment (like 1.2.0 → 2.0.0).

Pandoc compatibility: 2.8-2.9

# Release 1.2.0

2019-12-17

The `pandoc-unicode-math` filter now adds sensible whitespace where necessary:
The Unicode sequence `λx` now translates to `\lambda x` (note the added
whitespace) because the previous `\lambdax` would raise a Latex error, but `αβ`
still translates to `\alpha\beta` because this is unambiguous. Idea by Fynn
Leitow.

Several new symbols were added. Contributed by Fynn Leitow.

Pandoc compatibility: ca. 1.19-2.7.3

# Release 1.1.0

2019-04-09

A second filter `pandoc-unicode-math-from-latex` was added that replaces
"regular" Latex math commands by equivalent Unicode symbols. This is more or
less the inverse of the existing filter `pandoc-unicode-math`. Idea by Anish
Mittal.

Several new symbols were added. Partly contributed by Eric Hanson.

Pandoc compatibility: 1.19-ca. 2.7

# Release 1.0.0

2018-02-27

Initial release.

Pandoc compatibility: 1.17
