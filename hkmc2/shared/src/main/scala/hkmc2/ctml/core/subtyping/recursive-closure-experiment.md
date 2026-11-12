# A family of solutions for a recursive bound

This experiment replaces a recursive variable with an ordinary CTML type that represents **all**
solutions of its lower bound. It does not guess the variable from the shape of the target.
The implementation is on `experient/subtype-closure`, based on `81305500a` on `ctml`, without the
producer-specific changes from `experient/subtype-recursion`.

Enable it with `:ctml-cfg recursive-closure`; disable it with `no-recursive-closure`. It is off by
default. The focused examples are in `ctmlRecursiveClosure.mls`.

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
three failing files. There were no newly failing previously successful examples. These survey
golden changes are not part of the experiment.

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
