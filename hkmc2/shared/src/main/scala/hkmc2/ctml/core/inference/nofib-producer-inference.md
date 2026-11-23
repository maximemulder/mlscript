# Unannotated Nofib range inference

## Using positive recursive producers

For a scheme `∀X. {F(X) ≤ X} ⇒ B(X)`, implicit elimination can choose
`R = ∀Y. {F(Y) ≤ Y} ⇒ Y`. When F is monotone, R satisfies the guard. When B is covariant,
B(R) is below every admissible instance. The checker applies this equivalence before opening the
target. Several lower bounds on the same X correspond to closure under their union. The checker
keeps all of them in R and preserves guards independent of X around the resulting instance.
It checks variance structurally through operators, binders, and nested guards. Other quantifiers bind the
same parameters around the result. The identity body already denotes R.
The [fold derivation](../subtyping/recursive-encoding.md#fold) gives the finite proof.

The abstract producer comparison, Power's `list() as Series`, PrimeTest's inline
`takeLz(2, iterate(x => x + 1, 1))`, and Nofib's implicit map/range and take/range compositions
check. The PrimeTest prefix has integer elements and fails a string-list assertion. The focused
subtyping tests also exercise contravariant uses, an additional guard, and negative recursion.

Mandel2's `build_tree` infers four result carriers with the same lower bound, one for each
recursive child position. PrimeTest's `rands` infers two. For guards `H(Xs) ≤ Xi` with a common
monotone H, let M be the intersection of an admissible tuple Xs. Then
`H(M, …, M) ≤ H(Xs) ≤ M`. The diagonal tuple is admissible and gives a smaller or equal
covariant result. Conversely, every admissible diagonal is already an instance of the original
scheme. The checker can therefore close the single diagonal layer with the usual carrier rule.
The inferred tree checks against `MandTree` and the random stream against `IntStream`.

The same reasoning allows a private base Bi for each Xi. For bounds
`Bi ∨ H(Xs) ≤ Xi`, close the unary layer `Z ↦ H(B1 ∨ Z, …, Bn ∨ Z)` at its least carrier R.
The tuple `Xi = Bi ∨ R` satisfies every bound by fold. For any admissible tuple with intersection
M, each `Bi ∨ M ≤ Xi`, so monotonicity gives `H(B1 ∨ M, …, Bn ∨ M) ≤ H(Xs) ≤ M`.
It follows that `R ≤ M` and the constructed tuple is componentwise least.
In Pretty’s `mkIndent`, the suffix bound `A ≤ X ∧ Y` supplies a common base. A string-list
suffix yields a string list. An integer-list suffix fails the same result check.

The checker uses Bekić elimination for monotone systems of lower inequalities. With the other
coordinates Y held fixed, it closes one coordinate X at its least admissible value W(Y). It
substitutes W(Y) in the remaining inequalities and in the result. Shrinking X this way preserves
the other defining inequalities by monotonicity. A residual guard such as `X ≤ U` stays around
the result with W(Y) substituted for X. The structural variance check requires that shrinking the
chosen coordinates preserve every such guard. In `F(Y) ≤ X, G(X) ≤ Y`, eliminating Y gives
`X = μZ.F(G(Z))` and `Y = G(X)`. Implicit and explicit instantiation both check.

The representation affects which proofs CTML finds. Exact upper intersections identify owners
of lower bounds. Exact lower unions expose summands that can be shared among owners. Naming a
covariant subexpression E adds a coordinate H with `E ≤ H`. Every original solution extends
by taking `H = E`, and shrinking H to E preserves its uses. Elimination can thus close a cycle
at a shared subexpression, including the common layer of `mkIndent` or the tuple argument of
several different constructors in `ctmlCarrierStress.mls`. The checker counts edges into each
subexpression and names repeated constructor expressions that depend on a carrier. It tries a
strongly connected component at a time in dependency order. Before substituting a witness, it
counts its size at each free occurrence, up to 4,096 unfolded syntax nodes. A component that
exceeds this budget keeps its quantified parameters and guards while other components are tried.

For a nested type `{L ≤ U} ⇒ B`, assume the guard at the larger instance. If L shrinks and U
grows at the smaller instance, transitivity supplies its guard. Covariance of B then gives
the result comparison. This makes L and B covariant and U contravariant in this constructor.
An enclosing arrow domain or negation reverses all three signs. The same check applies when
a closed guarded type is written inline beside a carrier.

Mandel has an independent precision problem. In its integer reduction, extract the step as
`step(c, self) = delay(() => LzCons(c, mapLz(z => z + c, self())))`. The inferred callback
requires a stream of empty nodes. A callback returning a general integer stream fails. Explicit
integer arguments to mapLz make that callback check. An experiment propagating enclosing variance
through constraint operands in both the simplifier and dependency analyses also makes the implicit
step check. Closing the recursive orbit then exhausts 20,000 solver calls. The same variance
experiment takes Sorting's lines function past its budget and Constraints past its file timeout.
These outcomes concern the separate experiment.

The remaining sections record the baseline investigation and its original test counts.

## Baseline result

The inclusive `enumFromTo` from the Nofib prelude already infers a compact recursive constrained
type without parameter annotations, a result annotation, or explicit type arguments. Using that
type exposes incompleteness in implicit instantiation and subtyping. Supplying a signature to the
definition would hide this distinction.

The implementation of inference and subtyping is unchanged by this investigation. The executable
reproductions are `ctmlRecursiveProducerInference.mls` and `ctmlRecursiveProducerSubtyping.mls` in
the CTML test directory. Valid comparisons that fail are marked `:fixme`; the consumer that exceeds
the test time limit is commented out with a `FIXME`.

## Producer and inferred type

The source is `hkmc2/shared/src/test/mlscript-compile/nofib/NofibPrelude.mls`:

```mlscript
fun enumFromTo(a, b) = if a <= b then a :: enumFromTo(a + 1, b) else Nil
```

The reproduction replaces the constructor spelling with the existing CTML constructors and
declares integer comparison as a primitive:

```mlscript
data class LNil()
data class LCons(head, tail)
val (<=): Int -> Int -> Bool

fun enumFromTo(a, b) =
  if a <= b then LCons(a, enumFromTo(a + 1, b)) else LNil()
```

Let

```text
F(X) = LNil ∨ (LCons ∧ {head: Int} ∧ {tail: X})
R    = ∀X. {F(X) ≤ X} ⇒ X
```

The inferred type, up to renaming and parentheses, is exactly

```text
∀X. {F(X) ≤ X} ⇒ Int → Int → F(X).
```

The implementation therefore retains the recursive relationship in a finite constraint. A
top-level binding `val inferredRange = enumFromTo(1, 5)` also succeeds and generalizes to
`∀X. {F(X) ≤ X} ⇒ F(X)`. Both string arguments are rejected. These tests check typing, not execution
or termination; the source's inclusive endpoint and empty-range branch are preserved syntactically.

The inferred type is sufficient for the desired producer type. Instantiate `X` at `R`. The guard
becomes `F(R) ≤ R`, which is the fold theorem for this monotone encoding, and the result `F(R)` can
then be widened to `R`. The executable control

```mlscript
enumFromTo[IntList] as (Int -> Int -> IntList)
```

succeeds, as does its application to two integers. These explicit arguments belong to diagnostic
controls; the producer definition remains unannotated.

## Failure 1: solving an instantiated recursive bound

Both of these automatic uses fail:

```mlscript
enumFromTo as (Int -> Int -> IntList)
enumFromTo(1, 5) as IntList
```

The same problem occurs without terms, constructors, or a concrete list representation. For the
checker’s covariant abstract type application `F`, define

```text
R = ∀X. {F(X) ≤ X} ⇒ X
P = ∀X. {F(X) ≤ X} ⇒ Int → F(X).
```

`P ≤ Int → R` fails, whereas explicitly instantiating a declared `P` at `R` succeeds. Widening the
body of `P` from `F(X)` to `X` does not repair automatic instantiation. A nonrecursive control,
`(∀X. {Int ≤ X} ⇒ Int → X) ≤ Int → Int`, succeeds.

In `subtyping/constraining.scala`, `subtypeUnivSub` instantiates the left quantifier with a fresh
flexible variable `α`. Solving its guard records `F(α) ≤ α`. Comparing the result with `R` introduces
a rigid `β` with assumed bound `F(β) ≤ β`. The failing derivation has this shape:

```text
F(α) ≤ β
  F(α) ≤ F(β)
    α ≤ β
      F(α) ≤ β   -- repeated judgment through a rigid bound
```

`subtypeFlexVar` checks the opposite bound before recording the new bound. Consequently the
search revisits the original judgment before it has found the valid solution `α = R`. The trail
rejects that repeat because neither side is a flexible variable. This does not establish that the
constraint is inconsistent; it shows that this search cannot find its witness.

Accepting arbitrary rigid cycles would be unsound. The existing trail documentation gives the
counterexample `A ≤ B ∨ Str`, `B ≤ A ∨ Str`, which does not imply `A ≤ Str`. A solution needs an
actual witness or another justified constraint-solving rule, not unconditional cycle acceptance.

## Failure 2: splitting a closed carrier before instantiation

The generalized value `inferredRange` also fails its comparison with `IntList`. A declared type
with the same scheme reproduces this failure, independently of inference:

```text
(∀X. {F(X) ≤ X} ⇒ F(X)) ≤ R.
```

For concrete lists, the right introduction first supplies an arbitrary closed carrier `Y`.
The choice rules then expose its lower bound `LNil ∨ Cons(Int, Y)` before instantiating the left
scheme. The search tries to fit the entire left scheme into one constructor alternative, losing
the whole carrier that could serve as a witness. Choosing `X = Y` directly gives a finite proof:
both the guard and the body comparison are the assumed `F(Y) ≤ Y`.

The existing `subtypeTargetInstance` rule only handles a universal whose guarded body is exactly
its bound variable. It does not handle this `F(X)` body. The abstract application version of this
comparison succeeds, so the representation and ordering of choice rules matter independently of
the recursive-bound failure under arrows.

## Consumers expose additional work

An unannotated nonrecursive `firstOrZero` infers a type and works on each concrete constructor.
`firstOrZero(enumFromTo(1, 5)) as Int` fails. Its trace tries to discharge branch guards for the
whole argument instead of separate constructor cases. `subtypeUnivCases` in
`subtyping/negation.scala` explicitly declines case splitting when the argument contains flexible
variables, as this application does. This is a separate obstacle to composing inferred producers
and consumers; choosing the recursive witness alone is not established to solve it.

The isolated unannotated recursive consumer

```mlscript
fun length(xs) = if xs is
  LNil() then 0
  LCons(_, tail) then 1 + length(tail)
```

exceeds the local harness's 25-second limit even without an application. Its exact performance
bottleneck is not localized here. It is retained as a commented reproducer, not an active timeout.

## Deferred solver investigation

This investigation is deferred. The Nofib port tests inference and can retain these failures;
making implicit instantiation succeed is not a prerequisite for translating more source code.
Continue according to the
[Nofib porting guide](../../../../../../test/mlscript/ctml/nofib/README.md).
The following is a possible direction for later solver work, not an approved implementation task.

Keep the inferred constrained scheme and make its implicit uses work. Investigate witness
selection for positive recursive bounds: for `F(α) ≤ α`, the encoded least fixed point is a
candidate, and an available closed target carrier is another candidate. This is an algorithmic
design change requiring review before implementation.

Any rule must establish monotonicity where it uses the fixed-point theorem, respect variable scope,
check every remaining lower and upper bound, and preserve the obligations of the enclosing
expression. It must not treat failure to discover a witness as a proof of inconsistency or accept
rigid cycles as proofs. The preexisting cyclic-constraint soundness regression documented in
`../subtyping/recursive-encoding.md` remains relevant to validating such a rule.

Use the automatic producer comparisons above as the first acceptance tests, then the composed
consumer and unannotated `length`. General implicit impredicative inference is a broader problem;
these examples do not establish completeness or prescribe a solution for arbitrary recursive
constraint systems. No change to the declarative type language is needed for the explicit finite
proofs exhibited here.

## Validation

The baseline CTML suite passes all 47 files. With the two diagnostic files, all 49 files pass in
about four seconds, with seven expected failures explicitly marked `:fixme`. Each new file runs
in under two seconds in the focused run. The isolated `length` timeout is excluded from execution.
Existing golden files are unchanged. No checker, shared infrastructure, Nofib benchmark, paper,
or Lean implementation is modified.
