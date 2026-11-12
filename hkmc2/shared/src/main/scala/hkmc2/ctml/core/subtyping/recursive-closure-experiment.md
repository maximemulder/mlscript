# Algebraic recursion follow-up

The branch `experiment/algebraic-recursion` starts from `experient/subtype-closure` (`a27e7cc67`).
It keeps ordinary expression inference and context joins. There is no summary builder, second body
checker, guessed recursive contract, or rule that proves a judgment by revisiting it.

Enable the additional normalization with `:ctml-cfg algebraic-recursion`; disable it with
`no-algebraic-recursion`. It is off by default. The existing `recursive-closure` option remains on
by default and must be on for either normalization to run. The pending-bound correctness fix is
independent of these options.

## Results and limits

`ctmlAlgebraicRecursion.mls` contains both enabled and disabled controls. In particular, ordinary
inference gives the unannotated recursive producer

```mlscript
fun replicate(n, x) = if n == 0 then LNil() else LCons(x, replicate(n - 1, x))
```

an ordinary constrained polymorphic type with a payload variable and a recursive result carrier.
Both `replicate as (Int -> Int -> IntList)` and `replicate(2, 1) as IntList` succeed with complete
problem normalization and fail with the inherited single-binder closure. The Boolean-payload
negative test is rejected. The inferred source type itself is unchanged: the improvement is in
solving its constraints when using it.

Declared consumer schemes with `A ≤ Stop ∨ (Next ∧ {tail: A})` also accept finite chains, while
rejecting Boolean inputs and malformed tails. These examples test solving a supplied recursive
constraint, not inference of the corresponding source consumer.

Unannotated recursive consumers remain unresolved. A unary traversal exhausts the focused
2,000-call budget; even adding an `Int` result annotation exceeds 5,000 calls. Live `:fixme` cases
record these limits. Calls are commented out because the definitions have no inferred type. The result is a foundation and a
producer improvement, not a replacement for all the practical successes of `fix-recursion`.

## Parameterizing upper bounds

The lower-bound family described below covers `F(a) ≤ a`. A consumer domain instead often needs
`a ≤ F(a)`. For monotone `F`, put `G(x) = ¬F(¬x)` and use

```text
Interior_F(b) = ¬μX. (G(X) ∨ b).
```

Every member satisfies the upper bound. Every solution `a` is represented by choosing `b = ¬a`.
The seed denotes excluded values here; it is not a greatest-solution-only choice. As with lower
closure, substitute the witness in the entire result and every residual constraint. Opposite
bounds, unsupported guards, and negative uses in the remaining problem are retained.

`RecursiveClosure.lean` adds `upper_closed`, `upper_recover`, and `upper_solutions`, checked against
the sibling mechanization. Their explicit premises are complement duality (`AreDual`), the scoped
monotonicity needed for the lower closure (`CanClose`), monotonicity of `F`, and congruence of the
residual predicate. The proof does not verify the Scala variance checker or establish these premises
for every Scala type form. An independent finite-model test evaluates the actual generated ASTs,
including universal quantification and guards, for all 8,000 monotone operators on three atoms and
all eight seeds, in both directions; the represented solution sets agree exactly.

## Complete local problems

The inherited integration only examines the first universal binder. For a producer polymorphic in
its payload before its recursive carrier, that binder has no self bound, so normalization misses
the carrier. `RecursiveProblem` exposes all leading universal binders, the result, and all guards
as one local problem. All binders are freshly opened at the same existential level. Each original
variable is considered once, with lower bounds preferred; if none can be closed, upper closure is
tried. Other declarations are available as parameters, but their bounds are not assumed while
certifying a transformation. Each substitution covers the whole remaining problem. Seeds are not
recursively transformed on the same pass.

This is sequential elimination, not a general algorithm for mutually recursive constraints.
Substitution can put a remaining recursive occurrence beneath a universal or guard; such a bound
stays unsupported. Variables already spread through an ambient inference context still cannot be
rewritten through this API. The downstream solver remains incomplete even when normalization
preserves all mathematical solutions.

## A necessary propagation invariant

The experiment exposed an independent solver bug. Consider these three constraints, with distinct
constructor tags:

```text
Bool ≤ b
Next ∧ {tail: b} ≤ a
a ≤ Stop ∨ (Next ∧ {tail: a})
```

They are inconsistent: the recursive upper bound forces the tail into `a`, so `Bool ≤ a`, while
`a` excludes `Bool`. During checking of the upper bound against the current lower bound, propagation
can discover `Bool ≤ a`. If the pending upper bound is invisible during that check, the new lower
bound is checked against `Top` and the inconsistent interval survives.

`subtypeFlexVar` makes its pending structural bound visible during opposite-bound propagation.
Failure returns no clauses. Redundancy is checked against the incoming context, since propagated
bounds may rely on the pending assertion. The unit test checks both a valid `Stop` leaf and an
invalid `Bool` leaf in all six constraint orders. This also makes the existing
`ctmlNestedListResult.mls` string-in-integer-lists counterexample fail as it should, while its valid
integer control continues to pass; its `:breakme` marker is removed.

## Validation of the follow-up

The unmodified base passed all 60 CTML diff-test files. The follow-up passes 61 files (including
its new focused file) and all 50 CTML unit tests. A full-suite survey with algebraic normalization
on by default also passed all 61 files; its changes were debug variable numbers and diagnostic
search output in three files. The checked-in configuration and goldens retain the opt-in default.
The Lean proof checks with the command recorded below, with no added axioms or unfinished proofs.

The new focused file took about 3.2 seconds in the full run and 1.8 seconds in its final
focused run, including the additional bounded unannotated traversal probe. The existing provenance-consumer file remains slower than the desired
five seconds (about 6.7 seconds in that run, versus 5.6 seconds in the initial baseline); no file
reached 30 seconds. These concurrent test timings are observations, not a benchmark.

Golden changes outside the new file reflect the repaired nested-list rejection, longer diagnostic
paths, search accounting, and retaining an additional bound in the `g9` recursive self-application
example. Its `g10` omega control remains `Bottom`. Two provenance unit tests now assert bounded
search work and the required cut/fallback behavior rather than exact allocation/cut counts; the
pending-bound consistency checks can visit additional alternatives.

## Investigated alternatives

The discarded probes are recorded here to distinguish experimental evidence from the retained
implementation:

- A fresh local recursive variable alone inferred a large guarded type, then exhausted the budget
  when applied to a nonempty list.
- A fresh arrow assumption and monomorphic abstraction spine did not resolve the cycle through
  branch guards. Generalizing the branch-sensitive abstraction remains significant: different
  input cases can require different instances.
- Deferring the recursion equation as raw solved bounds was unsound: simplification dropped
  obligations and accepted a wrong return type. Keeping it as an explicit guarded obligation
  preserved the obligation but did not produce usable nonempty-list consumer types.
- Flexible-domain case splitting, Boolean variable isolation, and structural universal witnesses
  did not make those consumer probes tractable. None is included here.
- Making variable-to-variable bounds pending as well regressed the omega self-application test;
  without provenance cuts that probe overflowed the stack. That broader change is not included.
  The retained structural-bound fix preserves the omega regression's `Bottom` result.

The next research step is solving the finite guarded recursive problem produced by a step function,
with independently instantiated input cases and shared recursive invariants. Extending closure to
positive upper bounds does not by itself discharge these first-class guarded dependencies.

---

# A family of solutions for a recursive bound

This experiment replaces a recursive variable with an ordinary CTML type that represents **all**
solutions of its lower bound. It does not guess the variable from the shape of the target.
The implementation is on `experient/subtype-closure`, based on `81305500a` on `ctml`, without the
producer-specific changes from `experient/subtype-recursion`.

Enable it with `:ctml-cfg recursive-closure`; disable it with `no-recursive-closure`. It is on by
default at the base commit of this follow-up. The focused examples are in `ctmlRecursiveClosure.mls`.

## What works

The following checks succeed with the experiment enabled:

- A producer returning a recursive layer can be checked against a recursive result type.
- The same bound works inside a record field or a tuple, without a separate rule for those shapes.
- List layers containing a union, constructor tags, and fields work too.
- `enumFromTo as (Int -> Int -> IntList)`, `enumFromTo(1, 5) as IntList`, and an intermediate
  `val inferredRange = enumFromTo(1, 5)` all work without explicit type arguments.
- A consumer can choose a carrier larger than the smallest recursive type. Additional upper
  bounds are still checked; contradictory bounds fail in either order.
- Parameterized list elements can use a scoped assumption `A ≤ B`. That assumption does not make
  the same comparison succeed outside its scope.

The negative tests reject wrong range arguments, wrong list elements, malformed tails, and
incompatible extra guards. Negative recursion is not given a fixed-point law.

## The transformation, in plain terms

Suppose a local inference variable `a` must satisfy `F(a) ≤ a`. For a list, `F(a)` describes either
an empty list or a cons whose tail belongs to `a`. The bound says that `a` contains those values.

Choosing just the smallest such type would lose valid solutions. For example, a function accepting
`a` might need `a = Top`. Instead, introduce a fresh variable `b` for any extra values to include:

```text
Close_F(b) = μX. (F(X) ∨ b)
          = ∀X. {F(X) ∨ b ≤ X} ⇒ X
```

Here `μ` is shorthand for the existing universal-and-guard encoding, not new type syntax. This
type contains `b`, and also everything that must be included to satisfy the recursive bound.

Replace `a` with `Close_F(b)` everywhere in the local problem: its result type and both sides of
every remaining constraint. Remove the lower bound that the closure guarantees. Keep all other
constraints. Several supported lower bounds are first combined by union.

For example:

```text
F(a) ≤ a, a ≤ U, result = a -> a
```

becomes:

```text
Close_F(b) ≤ U, result = Close_F(b) -> Close_F(b)
```

The remaining bound still needs solving. Nothing here establishes that the complete problem is
consistent, and the two occurrences of the carrier share the same seed.

## Why it preserves the solutions

Assume `F` is monotone in the contexts required by the encoded fixed-point theorem.

1. Every `Close_F(b)` satisfies `F(Close_F(b)) ≤ Close_F(b)`, by the existing fold theorem.
2. Every solution `T` is represented: choose `b = T`. Fold gives `T ≤ Close_F(T)`, and the
   closed-carrier elimination rule gives `Close_F(T) ≤ T`.

Thus no solution is lost, up to mutual subtyping. In particular, the remaining problem need not
be monotone in `a`: it only needs to respect equivalence of types. Negative occurrences are fine.
The recursive body `F` itself still needs the monotonicity premise.

`RecursiveClosure.lean` checks this argument against the sibling CTML mechanization. It proves
closure, containment of the seed, leastness, recovery of each solution, and:

```text
(∃ a. F(a) ≤ a ∧ D(a)) ↔ (∃ b. D(Close_F(b)))
```

The theorem takes the equivalence-respecting property of `D` as an explicit premise. It also
proves the required scoped monotonicity premise for list layers with an arbitrary element type.
There are no new axioms or unfinished proofs. This proves the mathematical transformation, not
the Scala implementation or the entire constraint solver. The general Scala variance test and
congruence of every form of residual CTML problem are not mechanized here.

To check the proof using the existing sibling build:

```sh
cd /media/Data/HKUST/Research/ctml/lean/CTMLCore
lake env lean /media/Data/HKUST/Research/mlscript/hkmc2/shared/src/main/scala/hkmc2/ctml/core/subtyping/RecursiveClosure.lean
```

The paper and sibling Lean source files are unchanged. The proof file lives with this experiment.

## Where it fits in the checker

`closeRecursiveVariable` accepts an already opened, locally owned flexible variable, all its
constraints, and its result. It does not receive a target type. It asserts that the ambient context
does not mention that variable, checks declaration levels, and uses capture-avoiding substitution.

The current integration opens one universal binder after conjunctive splits and before choosing
a target union branch. The two boundaries both matter. Different target conjuncts may need
different instances: `∀X. {tail: X} ≤ X ⇒ X -> X` can be checked against both `Top -> Top` and
`{tail: Top} -> {tail: Top}`, but one carrier cannot satisfy both arrow comparisons. Conversely,
choosing a target union branch before closure loses the whole recursive bound too early.
Applying an inferred producer works because the variable is closed when the function is first
instantiated, before its result flows into later checks.

This does **not** yet collect an arbitrary variable's bounds out of an existing inference context.
The unit test for an already opened variable supplies its complete local problem explicitly.
An older variable whose uses are spread across the context cannot safely be rewritten by this API.

The accepted recursive bodies use the structural variance rules for unions, intersections,
immutable fields, tuples, arrows, negation, and CTML's covariant applications. A recursive
occurrence under another universal or a constrained type is unsupported. Subexpressions independent
of the recursive variable may contain either. A lower bound outside this fragment remains an
obligation; it is never silently discarded.

Unsupported instances use ordinary subtyping. Supported instances use the transformed problem;
the ordinary solver may still fail to find a valid seed or proof. During that check, further closure
transformations are disabled on the current path so the generated encoding does not repeatedly
transform itself. This also limits nested opportunities for the experiment.

The experiment adds one entry point to `constraining.scala`; the transformation and variance check
are in `recursiveClosure.scala`. There are no function-result, record, list-name, or target-witness
choices in the transformation. The existing identity-body target-instantiation rule still helps
check the generated encoding. Its preference for the first successful conditional candidate is
unchanged; the closure theorem does not establish completeness of that remaining search.
No repeated judgment becomes a new coinductive proof rule.

## Validation and measurements

The baseline has 59 passing CTML diff-test files. The new file adds focused opt-in checks. The unit
suite also checks substitution at both polarities, residual constraints, guard order, scope and
level rejection, independent instances for target conjuncts, an explicitly opened problem, and a
concrete seed satisfying every asserted residual bound of a producer. A finite sanity test covers
all 8,000 monotone maps on a three-atom powerset; each closure family represents exactly the original
bound's solution set. The final checks pass all 60 diff-test files with the experiment enabled only
in its dedicated file, and all 43 tests in the three CTML unit suites.

```sh
sbt --client "dtest ctml/"
sbt --client "hkmc2JVM/testOnly hkmc2.ctml.*"
```

A full-suite survey with closure enabled under normal settings completed in 6.13 seconds. Its only
unexpected outcomes were eight existing `:fixme` cases succeeding: three producer subtyping cases,
three producer inference cases, and the Nofib `map(abs, enumFromTo(...))` and
`take(..., enumFromTo(...))` compositions. The harness therefore reported 57 passing files and
three failing files. There were no newly failing previously successful examples. The base commit includes these resolved expected-failure and golden changes.

A separate survey with a 10,000-call budget hit existing expensive searches. The same-budget
control with closure disabled hit the same 26 unexpected error blocks across nine files; enabling
closure added only the eight expected-failure successes. Those budget failures are not evidence
of closure regressions. Wall-clock times are observations from this machine, not benchmarks.

Focused measurements with `profile-search` enabled:

| Check | Solver calls, including optional checks | Maximum goal syntax nodes |
| --- | ---: | ---: |
| Abstract producer to `Int -> Rec` | 19 | 18 |
| Record wrapper to `{value: Rec}` | 18 | 18 |
| List layer scheme to `IntList` | 61 | 34 |
| Inferred range function ascription | 64 | 36 |
| `enumFromTo(1, 5) as IntList` | 160 | 36 |
| Carrier with an incompatible extra upper bound | 19 | 18 |

These operations had no exact repeats, provenance repeats, or search cuts. This is evidence of
small finite derivations for these examples, not a termination or complexity theorem.

## Remaining work

The result supports continuing with solution-preserving bound normalization. It does not yet
provide a general recursive subtype decision procedure. Mutual recursion, collecting complete
problems for existing variables, recursive occurrences inside first-class guards, and broader
proof search remain open. Inferring an unannotated recursive consumer's contract is still a
separate problem; this experiment does not solve it.

The next step should expose complete local constraint problems in the solver, so this same
transformation can apply wherever a variable originates. After that, finite representations and
proved fold/carrier operations could avoid repeatedly opening the encoded universals. Neither step
should introduce a rule that accepts arbitrary cycles of inequalities.

The family-of-solutions approach is inspired by the tallying solver of
[Castagna et al.](https://www.irif.fr/~gc/papers/polydeuces-part2.pdf). Its theorem does not directly
apply to CTML's first-class guards and impredicative quantification. The encoded monotone fixed
point also has a close precedent in
[Jenkins and Stump](https://arxiv.org/abs/2001.02828); the proof here uses CTML's own rules.
