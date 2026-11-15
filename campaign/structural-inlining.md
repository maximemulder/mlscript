# Structural obligations during alias substitution

Base: `93a0997a40087cb9dcf5373ed714bbe2677706c2` (current main baseline).
Branch: `experiment/campaign-20261115-structural-inlining`.

## Defect and rule

`Bound.inline` removes a reference to the bound's owner from the alias replacement before
substituting it. This is invalid beneath a structural constructor: with `T = R`, rewriting
`R <= {tail: T}` produces `R <= {tail: Top}`; the lower-bound dual produces `{tail: Bottom}`.
The independent baseline fixture fails on this loss, without any experimental recursion rule.

The repair reads the full replacement before hiding the owner's bounds during simplification,
then removes only direct reflexive operands of the completed bound. Thus `R >= T | {tail: T}`
becomes `R >= {tail: R}`. The direct `R >= T` case still becomes a tautological lower bound.
Constructor positions and contravariant arrow parameters keep their recursive obligations.

This is a substitution invariant, not a coinductive proof rule. Owner-bound masking remains:
simplification must not erase a nonrecursive seed by using the very bound being rewritten.

## Evidence

- `baseline-inline.log`: independent main-base regression, two failed tests and one passing
  direct-alias control. A preliminary test compilation missed an import; the preserved run
  contains the corrected fixture and no production changes.
- `fixed-unit.log`: all 35 CTML unit tests pass, including structural substitution in both bound
  directions, fields, tuples, arrows of both variances, applications, and mixed direct/structural
  occurrences. Command:
  `sbt --client "hkmc2JVM/testOnly hkmc2.ctml.core.simplification.StructuralInliningTest hkmc2.ctml.*"`.
- `fixed-ctml.log`: full CTML suite, 58/59 pass in 6.902 seconds. The single new failure is
  the valid `firstOrZero(unroll(pair)) as Int` call in `ctmlRecursiveLists.mls`.
- `fixed-known-regression.log`: focused recursive-list file passes after adding the explicit
  expected-failure marker, 2.329 seconds. This does not count as fixing the valid call. The
  full suite was not rerun merely to change that marker. The campaign server was stopped.
- Independent review requested negation, guarded occurrences, and self-elimination coverage.
  The expanded focused suite passes 4/4 in 195 milliseconds (`review-controls.log`), preserving
  both recursive occurrences and nonrecursive seeds. Production code is unchanged by this
  follow-up. The reviewer found no counterexample and agrees the union-input failure concerns
  completeness, not permission to erase the structural obligation.

## Precision cost and limits

The valid call now retains a self-dependent input guard that asks the entire union input to
fit one constructor branch. That loses completeness for a mixed constructor union. It is
explicitly marked `:fixme`; marking it is not an inference improvement. Other changed outputs
retain guards previously erased. `g2` in flow correlation changes from `Bottom -> Bottom` to
`Bottom -> Int`, retaining its empty domain and rejecting both tested calls.

The motivating rejected raw-recursion prototype accepted a constructor-valued result as `Int`.
Its trace exhibits the same structural loss, but rechecking that whole prototype with this
repair is separate validation and does not make that prototype an adoption candidate.

Recommendation pending independent review and combined validation: this repairs a concrete
obligation-preservation defect with a small rule change, but carries a documented completeness
regression. Do not describe it as regression-free or silently restore the erased obligations.
