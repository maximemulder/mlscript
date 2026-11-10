# Unannotated Nofib range inference

## Result

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
