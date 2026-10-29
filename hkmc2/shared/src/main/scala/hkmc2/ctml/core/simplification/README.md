# Type simplification

Simplification operates on either an open inference result `(Type, SubClauses)` or a type whose
variables and bounds are already available in a `SubContext`. It does not close polymorphism
levels: solving, wrapping constraints, and quantifying the remaining variables belong to
`inference/level.scala`.

## Entry points

- `simplifyLevel` first simplifies an open result, then simplifies its type under the remaining
  clauses. These passes run in this order so that type simplification sees the substituted bounds.
- `simplifyClauses` removes eligible local variables from an open result. It tries ordinary
  polarity inlining before elimination with recovered bounds, and restarts after each removal.
  Clauses store the most recent declarations first; candidates are tried from the oldest one.
  Each successful step removes a declaration, so the iteration terminates.
- `Type.simplify` recursively simplifies type constructors under a context. Constrained bodies
  are simplified assuming their guard; the guard itself is simplified in the ambient context.
- `eliminateLocalVars` prepares branch clauses for a context join. It discards effective bounds
  and eliminates branch-local variables before the join nests them in guards at both polarities.
  There is no result type at this boundary, so elimination uses `TTop` as an empty observation.

## Variable transformations

`inlineVars.scala` handles ordinary inlining. The result type's transitive polarities determine
which bounds of a variable matter. Mixed polarities require equal lower and upper bounds. An
additional check on the surviving clause occurrences prevents recursive replacements outside the
result type. Bounds in directions that do not matter to the result are excluded from the
replacement context.

`eliminateVar.scala` handles relations recorded on other variables: for example, `β ≥ α` also
provides the upper bound `α ≤ β`. After recovering these bounds, it requires the other occurrences
to have a single polarity. The variable is substituted in its own bounds first and then in the
remaining state, preserving constraints between its lower and upper bounds. Analysis and rewriting
are exposed as one `tryEliminateVar` operation so callers cannot forget the recovered bounds.

These strategies share the polarity-aware substitution in `inlineType.scala`, but their selection
of replacement bounds differs. Applying ordinary inlining to branch clauses with no result type
would treat every variable as unobserved and could lose relations needed by the join.

## Invariants and contexts

- Open transformations receive output clauses with no effective bounds. Level processing
  summarizes effective bounds; the branch boundary removes them because they may rely on branch
  assumptions. Both transformations assert this precondition and validate the resulting state.
- Candidates must be declared in the output clauses. Level simplification additionally checks
  their effective level and `NoInlineVars`, preserving dependencies on an enclosing scope.
- Bound substitution temporarily overrides the eliminated variable's effective bounds to exclude
  direct references to the variable being bounded. This prevents a relation between two variables
  from becoming a self-reference. Bound lookups must respect these overrides.
- Polarity selects the lower bound at a positive occurrence and the upper bound at a negative
  occurrence. Negations and function parameters reverse polarity. Constraints use the polarities
  of their subtyping direction independently of the enclosing type's polarity; changes to these
  conventions must agree with the analyses in `type_/impls` and `structural/getVarDeps.scala`.

## Rebuilding types

The `make*Type` constructors in `core/constraint.scala` perform local normalization. Simplification
uses them directly where that is sufficient. Context-dependent rules for joints, quantifiers, and
constraints are shared by recursive simplification and inlining.

A joint is checked for an operand equal to the other operand's effective bound both before and
after recursively simplifying its operands. The first check preserves the exact shape of the
recorded bound; the second handles bounds exposed by recursion or substitution. General
subsumption checks then delegate to `combine` and subtyping. Subtyping search is an incomplete
simplification oracle, so failure to establish a relation leaves the type intact.
