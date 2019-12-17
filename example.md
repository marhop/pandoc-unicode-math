# Notes concerning the $λ$ calculus

$λ$ terms:

  * Variable: $x$
  * Abstraction: $(λx.M)$
  * Application: $(MN)$

Reduction operations:

  * $α$ reduction: $λx.M → λy.[y/x]M$
  * $β$ reduction: $(λx.M)N → [N/x]M$

Church numerals:

  * $0 := λfx.x$
  * $1 := λfx.fx$
  * $2 := λfx.f(fx)$
