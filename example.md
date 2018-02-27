# Notes concerning the $λ$ calculus

$λ$ terms:

  * Variable: $x$
  * Abstraction: $(λ x . M)$
  * Application: $(M N)$

Reduction operations:

  * $α$ reduction: $λ x . M → λ y . [y/x]M$
  * $β$ reduction: $(λ x . M)N → [N/x]M$

Church numerals:

  * $0 := λ fx . x$
  * $1 := λ fx . fx$
  * $2 := λ fx . f(fx)$
