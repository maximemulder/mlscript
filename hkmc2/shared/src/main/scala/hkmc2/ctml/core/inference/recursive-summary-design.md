# Recursive summaries with local branch refinement

## Recommendation

Prototype candidate synthesis followed by checking the original recursive body against the
candidate. Keep constructor alternatives and local branch refinement; summarize recursive calls
with one shared contract. This is a restricted inference strategy, not a change to what programs
mean or permission to discard typing obligations.

Start with regular structural recursion over immutable tagged records. Generate candidates using
finite structural templates, encode positive recursive data shapes with the existing constrained
encoding, and check candidates before accepting them. Do not initially replace general subtyping
with an abstract interpreter or globally approximate context joins.

This report proposes a design for review. No inference or subtyping implementation is changed.
Automatic candidate synthesis, its soundness argument, and performance on unannotated `map` remain
work to do. The supplied-invariant controls demonstrate parts of the validation route, not an
implemented inference solution.

## Evidence and comparison with FCCTI+

The [performance investigation](recursive-consumer-performance.md) isolates the expensive step:
closing recursion through `fix` feeds the inferred function back into its guarded bounds. The
body with a supplied recursive parameter infers quickly. Ordinary field recursion without a
branch also infers quickly. These observations support targeting recursive summaries, but do not
establish that fresh instantiation is the dominant cost or that every branch history is enumerated.

FCCTI+'s `sections/terminating-inference.tex`, especially its Approximation and Termination
paragraphs, supplies two distinct ideas:

- `cond` joins possible results without tracking correlation with the Boolean condition.
- `Forall1`/`Forall2` share instantiations by an abstract call context. The chosen abstraction is
  the set of program-location marks; its finite range bounds instantiation diversity. `Var1`,
  `Var2`, and `Skip` propagate new bounds and avoid reprocessing recorded ones.

Local source reference:
`/media/Data/HKUST/Research/first-class-polym/constrained-types-paper-new/`.
The constraint rules are in `figures/small-steps-constraint-solving-rules.tex`.

CTML has guarded context disjunction, negation, and rigid assumptions beyond that constraint
language. Finite instantiations alone do not establish finite CTML bounds or a useful runtime
bound. Call-site sets can also have exponentially many values. Copying the instantiation cache
without abstracting guarded bounds is therefore not yet a justified fix.

The CTML paper's `sections/E-lean-status-report.tex`, subsection on join performance, independently
records duplication of nested guards and expensive failed searches. Its terminating solver variant
does not guarantee practical runtime. This supports separating termination, representation size,
and search cost rather than expecting a revisit cut alone to solve this problem.

## New checking experiments

[ctmlRecursiveSummaryChecking.mls](../../../../../../test/mlscript/ctml/ctmlRecursiveSummaryChecking.mls)
uses the existing checker without modifying its rules.

For unary traversal, define:

```text
F(X) = Stop | (Next & {tail: X})
Nodes = forall X. {F(X) <= X} => X
H = Nodes -> Int
```

The following comparisons matter:

1. Infer `step(self, xs)`, whose body returns `0` or `self(tail)`. Inference finishes, as in the
   earlier investigation. Checking that inferred scheme at `H -> H` fails quickly: the curried
   scheme's inner domain is not split into its constructor cases. This is an incompleteness
   result, not another timeout. It is retained as `:fixme`.
2. Supply `H` at `fix`, expose one layer of `Nodes`, and ascribe the recursive reference to `H`
   inside the body. This checks. A concrete two-cell call checks; both a malformed input tail and
   a recursive call on `false` are rejected.
3. For heterogeneous lists, a nested match on a head exposed as `Top` fails with `Top <= ~Int`.
   It is retained as a second `:fixme`. The current infer-then-constrain path tries to constrain
   the concrete scrutinee type rather than give the branch a locally refinable binding.
4. Inferring the same head refinement independently as `intOrZero(h)` succeeds, but composing
   its guarded scheme inside the supplied-invariant traversal is also rejected (`:fixme`). A
   separate control checks a `Top -> Int` summary of that helper before composition; that
   traversal and a mixed-element call both pass. Adding an
   arbitrary unrefined head to an integer is rejected. These helpers are diagnostic, not proposed
   rewrites of Nofib programs.

Consequently, candidate validation should be a checking judgment with an expected type and
branch-local refinements. Merely emitting a type ascription around the already inferred scheme
is insufficient with the current checker. Even the successful explicit-invariant idiom needs
additional checking support to preserve arbitrary nested refinement in the original body.

## Proposed algorithm and its proof boundary

### Candidate construction

Preserve recursive-binding identity in the CTML expression representation. `parseBinding` currently
lowers it immediately to an ordinary `fix` application, losing a convenient boundary at which to
select a summary. An internal recursive-binding node can retain that boundary without adding
surface syntax; its checking rule must remain derivable using the existing `fix` contract. Do not
recognize arbitrary user variables merely because their spelling is `fix`.

For the initial structural fragment, collect constructor alternatives, field requirements,
recursive argument flows, and result constructors from the body before serializing branch joins
into mutually guarded types. Use slots associated with source parameters/results/field positions.
When a recursive argument flows back to a parameter, connect the slots rather than adding another
copy of the body or its branch guards. Retain payload variables shared with callbacks.

These are candidate equations, not asserted facts. For example:

```text
walk:    D = Stop | (Next & {tail: D}); result = Int
length:  D = LNil | (LCons & {head: Top} & {tail: D}); result = Int
copy:    D = LNil | (LCons & {head: A} & {tail: D}); result = D
map:     D = LNil | (LCons & {head: A} & {tail: D})
         E = LNil | (LCons & {head: B} & {tail: E})
         contract = (A -> B) -> D -> E
```

`map` must retain the same `A` and `B` throughout the body; merging all payloads to `Top` would
lose the contract needed by callers. Generalize these variables outside the recursive binding,
with ordinary escape/level restrictions, and use one instance throughout its recursive calls.

For a single positive structural equation, select the existing least-fixed-point encoding
`mu F = forall X. {F(X) <= X} => X`. This is a deliberate domain restriction to a usable recursive
shape, not an equivalence between arbitrary `X <= F(X)` constraints and that encoding. Check
positivity/monotonicity syntactically for the initial fragment. Reject unsupported negative cycles
as candidates; do not extend the recursive encoding theorem to them. Multiple mutually recursive
shape equations need a separate encoding/design and are outside the first prototype.

A finite slot set alone does not bound expressions stored in slots. The prototype must also bound
its template grammar: source constructor alternatives and field slots, scalar/payload variables,
and references to existing slots, with canonical unions/intersections. Unsupported higher-order
recursive shapes should yield no candidate, not initiate unbounded template unfolding.

### Validation

Given a candidate `H`, check the original body with `self : H`, proving the generator has type
`H -> H`. Then instantiate the existing `fix` at `H`. This provides a small proof boundary:
candidate generation may be incomplete or heuristic, but cannot make a program acceptable without
a derivation checking its original body. An inferred contract is not a trusted declaration.
This is an intended soundness reduction to the checking rules, not a claim that the current checker
is proved sound: the known cyclic-constraint counterexamples remain relevant regression tests.

Checking must propagate expected arrow types into lambdas, check each reachable pattern branch,
and give pattern-bound values refinable local types. For a known input `D` and pattern `P`, the
branch reasoning concerns `D & P` and `D & ~P`; it must not assert `D <= P` globally. Exposing one
encoded layer uses the existing fold/unfold derivations, with their obligations retained.

At each recursive call, check all arguments against the shared contract and use its result type.
Do not export the caller's branch assumptions as assumptions of the callee. The callee can still
refine its own input when checking its body. For multiple parameters, check every parameter, even
if it is unchanged at the recursive call.

Every branch result must satisfy the candidate result; every operation and evaluated argument
must check even when its result is unused. Exhaustiveness remains required. A wrong candidate
may fail quickly or reach a resource limit; neither outcome is a proof of the candidate. A solver
budget is a useful operational backstop, but this proposal does not prove termination of CTML's
general validation judgment.

### Precision policy

Preserve ordinary context-disjunctive inference outside recursive boundaries. Inside a recursive
body, retain local refinements, but use the selected summary at recursive calls. Begin with one
summary per recursive binding; add a finite collection of constructor-indexed summaries only if
real examples need the extra precision. Multiple summaries introduce their own coverage and
cross-call obligations and are not part of the first prototype.

Some valid functions will require correlations across recursive calls that one summary loses.
Failing to infer them is an explicit precision tradeoff. Keep their original programs as inference
failures; support checked explicit contracts rather than weakening obligations or changing their
algorithms. A bounded fallback to the current inference path is an option for unsupported shapes,
but should not silently restore the same long timeout on every supported consumer.

## Alternatives considered

| Approach | Assessment |
| --- | --- |
| Simplify the completed guarded scheme into a summary | Attractive as a small change, but even supplying the right summary to existing subtyping fails on unary `step`. A sound type-directed abstraction still needs a proof and better validation. |
| Share polymorphic instantiations as in FCCTI+ | Useful for fresh-instantiation growth. Needs origin/context tracking and handling of rigid variables and guards; current evidence does not establish it as sufficient. |
| Finite abstract contexts plus worklist saturation throughout subtyping | General direction, but requires an abstract domain for CTML's guarded types, a simulation argument, and a finite bound vocabulary. Much larger than a first recursive-consumer prototype. |
| Cache completed subtyping judgments | Complementary optimization. Keys must account for assumptions, variable kinds, bounds and trail-dependent results; it does not select the desired recursive invariant. |
| Drop guards or combine all branch requirements indiscriminately | Unsuitable. Dropping a guard can admit an invalid recursive argument; requiring the scrutinee to satisfy both disjoint constructor patterns makes useful domains empty. |
| Accept a repeated subtyping cycle | Unsuitable. Recursive calls justified by `fix` differ from circular subtype proofs; existing cyclic-constraint counterexamples still apply. |

## Suggested implementation sequence

1. Add expected-type checking for recursive-body validation, including branch-local bindings.
   Exercise supplied candidates first: unary `walk`, nested head refinement, invalid recursive
   arguments, invalid head operations, missing branches, and branch-local assumptions that must
   not escape. Reuse the existing rigid-element `map` control for payload relationships.
2. Preserve the recursive binding boundary, synthesize the smallest positive structural template,
   and validate it. Acceptance requires unannotated `walk` and `length`, meaningful result types,
   valid concrete calls and rejection of malformed tails. An empty domain is not success.
3. Extend candidate collection to reconstruction and callbacks (`copy`, then `map`), checking
   mixed-element calls and wrong callback/result types. Test that unrelated flow-correlation
   examples keep their precision.
4. Reassess Nofib `take`, `filter`, `max_`, and `addj` individually. Do not claim that these more
   complex examples follow from success on unary traversal.

The first stage is a checker improvement with explicit candidates; the second and third deliver
inference. This staged approach separates the correctness of checking a summary from the quality
of selecting one. Substantial checker changes remain proposals for review, as required by the
porting roadmap.

## Validation

The baseline has 53 passing CTML files. With the new controls, all 54 files pass in 5.8 seconds;
the focused file processes in 1.5 seconds. Passing includes the three explicitly marked checking
limitations, not successful inference of those programs. Existing golden files are unchanged.
No Scala code is modified. Run `sbt --client "dtest ctml/ctmlRecursiveSummaryChecking"` for the
focused controls and `sbt --client "dtest ctml/"` for the suite.
