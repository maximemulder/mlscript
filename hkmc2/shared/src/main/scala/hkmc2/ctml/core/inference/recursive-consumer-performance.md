# Recursive consumer inference: timeout investigation

## Finding

The sampled Nofib list timeouts occur while closing recursion through `fix`, after inference of
its function argument has finished. The expensive operation is subtyping the inferred, guarded
function scheme against `A -> A`. This feeds recursive input bounds back into the constraints
produced by context disjunction. Subtyping then repeatedly splits and checks guarded bounds,
including semantic checks used to simplify bounds.

The explicit constrained-polymorphic list encoding is not necessary to reproduce the problem.
The reductions below use only ordinary constructor declarations, patterns, and direct recursion.
They do not mention `IntList`, `restricts`, a manual library signature, or a producer application.
Changing the list representation is therefore not justified by these measurements.

This localizes a search bottleneck; it does not prove that the search is infinite, establish an
asymptotic complexity bound, or identify one optimization sufficient to make all Nofib functions
infer. The checker is unchanged. The executable controls and commented timeout reductions are in
[ctmlRecursiveConsumerInference.mls](../../../../../../test/mlscript/ctml/ctmlRecursiveConsumerInference.mls).

## Reductions and controls

Each timeout was isolated in a temporary diff-test file with its own constructors, using the
normal 25-second harness limit. Successful definition timings used `:ctml-cfg profile-infer`;
they exclude parsing and test startup and are indicative single-run measurements, not benchmarks.
Timing output is omitted from committed goldens because it is nondeterministic.

| Definition or control | Observation |
| --- | --- |
| Nonrecursive `first`, matching `LNil`/`LCons` | 66 ms |
| `walkStep(self, xs)`, returning `0` or `self(tail)` | 148 ms; inferred type size 79 |
| `lengthStep(self, xs)` | 51 ms; inferred type size 67 |
| `copyStep(self, xs)` | 85 ms; inferred type size 105 |
| `mapStep(self, f, xs)` | 239 ms; inferred type size 171 |
| Direct recursive `walk`, `length`, `copy`, and `map` | Each exceeds 25 seconds |
| Direct `length` with `dewrap-ucs-ifs` | Exceeds 25 seconds |
| Unary `Stop`/`Next(tail)` traversal | Exceeds 25 seconds |
| `spin(xs) = spin(xs.tail)`, with no branch | 84 ms; compact recursive constraint |
| Integer countdown | 16 ms; `Int -> Int` |
| Explicitly typed fixed-point traversal with an exposed list layer | Passes, including rejection of a malformed tail |

The smallest tested terminating-on-finite-inputs reduction is a unary traversal:

```mlscript
data class Stop()
data class Next(tail)
fun walk(xs) = if xs is
  Stop() then 0
  Next(t) then walk(t)
```

It removes list elements, arithmetic, reconstruction, higher-order callbacks, and nested patterns.
The equivalent two-field list reduction is:

```mlscript
fun walk(xs) = if xs is
  LNil() then 0
  LCons(_, t) then walk(t)
```

Replacing its recursive reference by a parameter gives the fast `walkStep` control. The same
comparison applies to `length`, `copy`, and `map`. These controls preserve the body but deliberately
leave the recursive invariant unsolved; they are not alternative benchmark implementations.

The nonterminating `spin` control infers

```text
forall X. {X <= {tail: X}} => {tail: X} -> Bottom
```

so recursive field bounds alone do not cause this timeout. Integer countdown also shows that a
branch and a recursive call are not sufficient by themselves. The problematic cases combine
structural recursion with the guarded relationships generated for constructor branches.

## Where the time goes

`parser/binding.scala` translates a direct recursive function to `fix (self => body)`. In
`core/inference/infer.scala`, the outer application first infers that argument and then constrains
`forall A. (A -> A) -> A` against `ArgumentType -> ResultType`.

A bounded trace of `fix(walkStep)` exposes the critical comparison independently of body inference:

```text
inferred walkStep scheme <= F -> F
```

After instantiating the scheme, its body has shape `S -> X -> R`, with `S` the supplied recursive
function, `X` the scrutinee, `T` the tail, and `R` the result. Arrow comparison creates

```text
F <= S
(X -> R) <= F
```

before discharging the scheme's guards. The joined upper bound of `S` describes either the empty
branch or a call `S <= T -> R` under the cons-branch constraints. Solving it propagates the
function's inferred domain back onto its recursive tail. The trace derives `T <= X`, then bounds
`T` by the guarded upper bound of `X`, including a field containing `T` itself.

This is a cyclic graph of guarded constraints, not an explicitly unfolded `IntList` alias.
`subtypeFlexVar` checks opposite bounds and then attempts semantic redundancy removal and bound
combination. Those checks rigidify the context and run subtyping again. Union/intersection
splitting, `joinMerge`, constructor case splitting, and further guard comparisons traverse the
resulting relationships repeatedly. Dependency traversal (`getVarTransDeps`) and structural type
hashing also appear in samples, but the samples do not establish either as the sole cause.

Eleven JVM thread snapshots per case, about two seconds apart, were captured for `walk`, `copy`,
`map`, dewrapped `length`, and unary traversal. In all 55 snapshots the test thread is inside
subtyping at the outer application, with `subtypeFlexVar` and `checkSubtype` on its stack. No
sample places the bottleneck in final type printing or final top-level simplification. Body
inference timings and the bounded trace independently support this localization. Stack presence
is not a CPU-time percentage; these are coarse samples rather than an allocation profile.

A temporary diagnostic replaced semantic redundancy checking and `combine` inside
`subtypeFlexVar` with syntactic `makeJointType`, while retaining opposite-bound checking and the
new asserted bound. `walk` still exceeded 25 seconds. This rules out removing those particular
optional checks as a sufficient fix. The diagnostic edit is reverted; other semantic checks
remain elsewhere in the search. No relaxed subtyping rule is retained.

## Distinct issues and limits

There are at least three separate obstacles:

1. **Closing recursive consumer inference:** the performance issue isolated here.
2. **Finding a recursive witness during implicit instantiation:** inferred `enumFromTo` cannot
   automatically satisfy some encoded-list contracts, although supplying `IntList` explicitly
   succeeds. See [the producer report](nofib-producer-inference.md).
3. **Using a list invariant in branch refinement:** the annotated direct `inferredLength` example
   in `ctmlRecursiveLists.mls` is rejected. Explicitly exposing a list layer and typing the
   recursive reference through `fix` checks the implementation. A source annotation alone is
   therefore not established to solve consumer inference.

The explicitly typed controls demonstrate a usable encoded representation, not completeness of
its implicit inference. They do not justify replacing unannotated benchmark functions with
signatures. The existing `take`, `filter`, `max_`, `addj`, and nested-comprehension timeouts are
consistent with the sampled family, but their individual hot paths were not profiled here.

## Recommended next work

Keep the list representation and manual library contracts while addressing the recursive
consumer search before adding more list-heavy ports. Start with `walk` and require an inferred
usable type, concrete valid calls, and rejection of malformed tails; then broaden to `length`,
`copy`, and the source `map`. A fast rejection or a trivially unusable domain is not success.

The next implementation investigation should reduce the expensive guarded comparison to a
standalone subtyping test, then examine repeated branch decomposition and bound propagation in
`subtypeFlexVar`, `splitAll`/`splitAny`, and `joinMerge`. In particular, distinguish required
constraint checks from optional simplification queries, and measure repeated judgments together
with their contexts and assumptions. The current trail tracks exact variable-side judgments on
a single search path; it is not a cache of completed searches across alternatives.

Any caching or subsumption change must account for context, rigid/flexible status, hypotheses,
and trail-dependent results. Do not discharge arbitrary cycles, discard branch guards, or treat
failure to find a witness as inconsistency. If the necessary fix changes recursive invariant
selection rather than merely avoiding redundant work, present that design separately for review.
The known cyclic-constraint soundness tests remain essential checks.

## Validation and reproduction

The unmodified baseline passes all 52 CTML files. The new diagnostic file passes in 1.7 seconds
including its setup. With the diagnostic file, all 53 CTML files pass in 6.2 seconds, and
existing golden files are unchanged. Timeout and debug-budget probes are excluded from active
execution; a
bounded debug trace reaching the 5,000-step limit is diagnostic evidence, not an expected type
error.

Run the active controls with:

```sh
sbt --client "dtest ctml/ctmlRecursiveConsumerInference"
sbt --client "dtest ctml/ctmlRecursiveLists"
```

To reproduce the timeout, copy the commented `walk` into a separate `:ctml` file with `LNil` and
`LCons`, leaving the encoded types and library imports out. To inspect closure of recursion,
define `walkStep` first, then put `:ctml-dbg constrain check depth 14` before `fix(walkStep)`.
This enables both constraining and checking traces; omitting `check` hides much of the sampled
work. The debug budget intentionally interrupts this experiment, so do not leave it active in
the regression suite. Stop the sbt server after testing.
