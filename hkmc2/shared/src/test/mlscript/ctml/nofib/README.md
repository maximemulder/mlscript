# Porting Nofib to CTML

## Purpose and priorities

The goal is to test CTML **type inference** on realistic programs. Port Nofib file by file,
preserving its algorithms and leaving ordinary function definitions unannotated. A faithful port
that exposes an inference failure is a useful test; successful inference is not a prerequisite
for continuing the port.

Read the repository's `AGENTS.md` for checker conventions and test instructions. This document
records the port-specific decisions and the handoff state. Update it when a port advances or a
design decision changes.

## Handling gaps

Distinguish these cases before changing the checker or rewriting a benchmark:

| Gap | Action |
| --- | --- |
| The language cannot express a source construct | Report the gap and propose a feature or a source rewrite to the user before implementing that decision. |
| The translated program is expressible but inference or subtyping fails | Preserve the unannotated program, record the failure, and continue porting. Defer solver work unless explicitly requested. |
| A standard-library type or operation is missing | Put shared support in `NofibPrelude.mls`; infer ordinary helper implementations and declare genuinely primitive operations with precise types. Discuss new representation choices with the user. |

Keep benchmark functions unannotated. For widely used library helpers whose inference failure
or timeout blocks downstream porting, a precise manual signature is an allowed fallback:

- Preserve the faithful unannotated implementation as an inference reproducer. Use
  `:ctml-parse-only // FIXME: <reason>` for timeouts and costly inference. Put shared reproducers
  in [preludeDeferred.mls](preludeDeferred.mls), which imports the checked prelude.
- Prefer checking the implementation against the supplied signature when feasible. Keep any
  annotated or explicitly instantiated diagnostic control separate from the unannotated port.
- When implementation checking is blocked or deferred, provide a documented trusted declaration in
  `NofibPrelude.mls`. State that its implementation is not checked by that declaration, and link
  relevant diagnostic controls. Do not replace its contract with an unchecked or dynamic type.
- Test callers against the supplied contract, including invalid arguments and result-type checks.
  Leave ordinary callers implicitly instantiated; retain independent inference failures as
  `:fixme` cases rather than weakening the signature to accept them.
- Report coverage as checking against manually supplied library signatures, not whole-program
  inference. Record each fallback in the status table below.

Result assertions and negative tests may check inferred definitions without replacing those
definitions with annotated ones. Solver work remains deferred unless explicitly requested.

Preserve source behavior, including range endpoints, empty cases, partial functions, evaluation
structure, and benchmark inputs. Document necessary representation changes. Do not replace a
list algorithm with arithmetic or another algorithm to avoid an inference limitation. CTML diff
tests check typing; a passing type test does not establish runtime equivalence.

The agent may choose the order of files and carry out routine translation within agreed decisions.
Report new feature-versus-rewrite decisions and substantial checker design changes to the user
before implementing them. An inference failure alone does not require such a design change.

## Agreed choices

- Add shared support as needed. Several benchmarks can be ported together when they fit the
  agreed conventions.
- Keep direct recursion. Mutual recursion and richer binding support are deliberately out of
  scope for now; revisit them only when a concrete source program warrants a decision.
- Represent triples as nested pairs, `[a, [b, c]]`, and destructure them with nested tuple
  patterns. CTML supports binary tuple patterns without spreads in `if ... is ... then` matches;
  direct parameter and `let` patterns remain outside this port's scope. Precisely typed trusted
  `fst` and `snd` primitives are also available in the prelude.
- Tuple-pattern fallthrough can expose missing product-disjointness subtyping. The focused
  `ctmlTuplePatterns.mls` tests retain these cases as `:fixme`; unconditional destructuring in
  the GCD kernel does not require these judgments.
- Represent source laziness with trusted polymorphic `delay` and `force` in the Nofib prelude.
  A handle has a nominal `Delayed` tag and carries its result type through the immutable
  `resume` function. Forcing an uncached handle evaluates its initializer and stores the result
  on success. Subsequent forces return the cached result. CTML tests check the handle and result
  types.
- Use readable constructor patterns for lists. An abstract list API with trusted `caseList`
  callbacks is not the chosen porting approach. CTML supports nonrecursive constructor
  declarations and constructor patterns; the list tail's recursive relationship is a type-level
  question rather than a recursive constructor declaration.
- Explore recursive lists through the existing constrained polymorphic encoding. The encoding
  and explicit-instantiation support are implemented; native recursive types are not a
  prerequisite for continuing the port. Generic source-level alias syntax or other missing
  conveniences still need a decision if a port requires them.
- Automatic recursive-witness selection and consumer inference precision remain open. The
  authorized consumer-performance investigation localizes the sampled timeouts to closing
  recursion over guarded bounds. Follow up on that bottleneck when expanding inference coverage.

## File-by-file workflow

1. Read the original benchmark and the helpers it uses. Identify its input, expected behavior,
   and dependencies before translating. Copy only the selected benchmark and needed helpers.
2. Translate into this directory using existing CTML constructs and approved rewrites. Record
   the original source path and representation choices in the file. Share reusable definitions
   through `NofibPrelude.mls`; CTML's `:import` already works.
3. Keep benchmark functions and ordinary helpers unannotated, except for the documented library
   fallback above. Classify each obstacle using the
   table above. For an inference failure, retain a small reproducer when useful, but do not let
   an open-ended solver investigation block the port.
4. Mark valid programs rejected by the checker with `:fixme // FIXME: <reason>`. Mark invalid
   programs accepted by it with `:breakme // FIXME: <reason>` followed by `:te`. These commands
   apply to diff-test blocks, so separate successful definitions from failing uses where useful.
5. Try inference on the translated definitions. For a timeout, isolate the offending block and
   add `:ctml-parse-only // FIXME: <reason>`. The flag runs MLScript elaboration and CTML
   translation for one diff-test block. Skipped definitions supply no typing bindings. If a
   dependency lacks a type, use a documented library signature where the fallback policy applies
   or keep its caller under the same flag when its names resolve. Keep terms with missing
   dependencies in comments with an explanation. The flag applies to ordinary test blocks and
   leaves imported preludes type checked.
6. Run focused tests, review the generated `//│` output, then run the CTML suite for executable
   changes. Follow `AGENTS.md` for baseline comparisons and any shared-code changes. Aim for
   under five seconds per file and skip inference for known timeout cases.
7. Update the status below with translated sections, active failures, disabled blocks, and the
   next concrete task. Distinguish translation completeness from successful inference.

Useful commands from the repository root:

```sh
sbt --client "dtest ctml/nofib/"
sbt --client "dtest ctml/ctmlRecursiveProducer"
sbt --client "dtest ctml/"
sbt --client shutdown
```

The runner rewrites golden output in place. Review `git diff` after a run, including expected
failures; a successful suite can contain explicitly marked inference limitations. Stop the sbt
server when finished. Do not include unrelated golden-output changes.

## Current status and next task

| File or area | State |
| --- | --- |
| [gcd.mls](gcd.mls) | Source translation complete, inference coverage partial: `g` and `gcdE` infer successfully. `max_` inference alone exceeds the 25-second limit; the extracted nested comprehension also times out. Both reproducers and the full driver check through translation with `:ctml-parse-only`. Independent checks of both tuple-mapping stages compose successfully through the trusted `map` contract, with an integer-list result assertion and an invalid-input test. |
| [cryptarithm1.mls](cryptarithm1.mls) | Source translation complete, inference coverage partial: `expand` and the exact ten-element `condition` pattern infer successfully. `addj` exceeds the 25-second test limit. It and its dependent permutation generation and driver check through translation with `:ctml-parse-only`. The source input remains `1`. The MLscript runtime reference reports a stack overflow, not a successful benchmark result. |
| [cse.mls](cse.mls) | All source definitions are present. The state combinators, `incr`, `newlyDefined`, tree constructors, and examples 0–3 infer without annotations. Example 4 checks with the trusted `scanl` contract. Six shared or local source list helpers and all four stateful traversals exhaust a 20,000-call diagnostic. The local list helpers, stateful traversals, tree labelling, `findCommon`, and example 5 check through translation. Graph construction, the visited-state check, and the driver are kept as comments because they depend on helpers unavailable in this file. |
| [scc.mls](scc.mls) | The complete SCC algorithm and input `0` are translated. The fixed graph and edge reversal infer unannotated, and transposing that graph checks against the trusted `map` contract. Neighbor lookup and DFS exhaust a 20,000-call diagnostic, even with temporary `inList` and `append` signatures for DFS. Neighbor lookup checks through translation. DFS, the dependent traversal, and the driver remain commented because they depend on `inList`, which is unavailable in this file. |
| [lcss.mls](lcss.mls) | The source algorithm and fixed progressions `(1, 2, 60)` and `(30, 31, 90)` are translated. The individual score update infers unannotated. Row traversal, initialization, split index selection, and recursive reconstruction exhaust the 20,000-call diagnostic. Reconstruction hits the limit even with temporary contracts for its dependencies. The row traversal, initialization, and split index selection check through translation. Reconstruction and the driver stay commented because reconstruction depends on unavailable list traversals. |
| [integer.mls](integer.mls) | The progression and nine arithmetic and comparison operations infer unannotated. A result assertion after matching the inferred progression retains an active `:fixme`. The nested traversal exhausts the 20,000-call diagnostic. The nested traversal, the two passes, and their driver check through translation with the adapted input `700000001`. |
| [secretary.mls](secretary.mls) | The infinite random-stream producer infers unannotated. A check forces two successive cells and recovers both integer scores. Lazy filtering and taking, deduplication, the strategy, and the trial and result traversals exceed bounded diagnostics. The per-seed simulation checks through translation. Deduplication and the lazy traversals are in `preludeDeferred.mls`. The strategy and result traversal remain commented because those helpers are unavailable in this file. The driver retains input `50`; source runtime output is `"[0.3,0.3,0.3,0.34,0.36]"`. |
| [life.mls](life.mls) | The cell rule, lazy repetition, and cell rendering infer unannotated. The forced repetition's outer shape checks, while a result assertion after matching it retains an active `:fixme`. A concrete call to `glue` checks its outer result shape using trusted `append`, and passing that result to `listLen` retains a `:fixme` for recursive instantiation. The seed row times out and core traversals exhaust bounded diagnostics. The seed, initial board, and row shifts check through translation. The generation and display pipelines remain in comments with input `15` and source runtime result `468`; they depend on helpers unavailable in this file. The empty-list case in `init` uses `raiseError("[]")`, matching the source rendering of `Nil`. |
| [calendar.mls](calendar.mls) | The Gregorian date arithmetic, fixed month tables, and blank-picture producers infer unannotated. Intermediate bindings keep month-table checking within the test budget. `firstDays` checks against trusted `scanl`, `map`, and `take` contracts. The picture dimensions `height` and `width` check on concrete pictures using trusted `listLen`. Justification functions retain active `:fixme` cases for the inferred `space` list passed to `append`. Later formatting stages and the input-`0` driver for 1993 are retained as comments. |
| [NofibPrelude.mls](NofibPrelude.mls) | Trusted memoizing delay/force, source string primitives, and fractional division of integer counts; trusted pair projections, strict equality `===`, `raiseError`, integer multiplication, quotient, and comparisons; inferred `head`, `null_`, `intRem`, `quotRem`, integer `abs`, and floor-division-based `intDiv`/`intMod`. Shared `LNil`/`LCons` constructors, inclusive `enumFromTo` and `enumFromThenTo`, `compose`, `intMax`, `replicate`, `iterate`, `enumFrom`, and `nofibStringToList` infer. Trusted polymorphic `map`, `take`, and `filter` signatures use the constrained recursive list encoding. Their unannotated implementations and the remaining shared traversals are checked through translation in [preludeDeferred.mls](preludeDeferred.mls). The trusted polymorphic `scanl` contract guarantees a nonempty result with the initial accumulator as its first element. The unannotated implementation exhausts a 20,000-call diagnostic. [A fixed-point control for rigid element types](../ctmlRecursiveLists.mls) checks the implementation against the supplied invariant. |
| Unannotated `enumFromTo` | Available in the Nofib prelude and infers a recursive constrained scheme. Endpoint-order and invalid-argument checks are in `preludeTests.mls`; diagnostic checks against the recursive list encoding still expose implicit-instantiation failures. |
| Manual `map` contract | Concrete-list calls check implicitly, including element-changing maps. Composition with inferred `enumFromTo` retains a `:fixme` for recursive instantiation in `preludeTests.mls`; an explicit range witness is tested separately as a diagnostic control. |
| Manual `append` and `listLen` contracts | `append` preserves the common element type, including unions for heterogeneous inputs. `listLen` returns an integer for a proper list of any element type. The unannotated implementations check through translation in [preludeDeferred.mls](preludeDeferred.mls). [Rigid-element fixed-point controls](../ctmlRecursiveLists.mls) check both implementations under supplied invariants. [Caller tests](preludeTests.mls) check composition, result types, and rejection of malformed arguments. The unannotated `concat` has an active `:fixme` because its inferred type accepts only the empty list. |
| List consumers | The focused investigation reproduces timeouts without encoded list signatures, down to unary constructor traversal. `walk`, `copy`, `map`, and dewrapped `length` samples locate the bottleneck in guarded-bound subtyping at the outer `fix` application. Function bodies with supplied recursive calls infer quickly; explicitly typed fixed-point controls pass. See `ctmlRecursiveConsumerInference.mls` and the performance report. Producer/consumer composition remains a separate precision issue. |

**Next port: `atom.mls`.** Start with the vector operations and reuse the memoized lazy-list
encoding for the simulation stream. Review the source numeric operations and formatting.

**Inference follow-up: make algebraically inferred recursive schemes usable without annotations.**
The [consumer performance report](../../../../main/scala/hkmc2/ctml/core/inference/recursive-consumer-performance.md)
localizes a timeout at recursive closure. Bounded CSE diagnostics also exhaust their search
budget before establishing that a traversal's inferred scheme accepts intended calls. Work on
both guarded-constraint search and scheme application, preserving branch refinements, source
recursion, and invalid-call rejection. Definition-only inference is insufficient evidence of
useful recursion support.

The historical
[recursive-summary design](../../../../main/scala/hkmc2/ctml/core/inference/recursive-summary-design.md)
compares FCCTI+-style instantiation sharing with candidate synthesis and body checking. Its
expected-type checking proposal is an alternative experiment, not the preferred implementation
direction. CTML's intended approach is algebraic inference without ordinary function annotations;
any departure needs a general semantic justification beyond improved test counts. The
`ctmlRecursiveSummaryChecking.mls` controls remain useful diagnostics of supplied-contract
checking and do not establish unannotated inference.

No new recursion rule is adopted by this port. Compare proposals on `walk`, `length`, `copy`,
`map`, and the stateful CSE traversals, including actual valid calls and genuine negative controls.
Keep resource exhaustion distinct from subtype refutation. Retain the existing recursive
representation, unannotated benchmarks, and documented library contracts. Substantial solver
design changes remain experiments for review.

CSE keeps the final benchmark input `6` and writes graph triples as nested pairs. Its
`throw Error(message)` branch calls the trusted `raiseError: Str -> Bot` primitive from
`NofibPrelude.mls`. The declaration models the absence of a normal return value. Rendering the
error message and the benchmark output still requires preserving the source list and triple format.
The direct Boolean parser support required by `mif` is checked in `ctmlBooleanConditions.mls`.

SCC preserves the edge and vertex order, both traversals, and input `0`. Its runtime
reference expects `"[[1],[2],[7,5,6],[3,4]]"`. The shared `inList` and `append`
implementations check through translation in [preludeDeferred.mls](preludeDeferred.mls). A separate check of
`LCons(LCons(1, LNil()), LNil()).toString()` fails because CTML's list constructors lack the
source rendering method. SCC and CSE both need the source list format for their entry
points.

LCSS and Integer share the inclusive arithmetic progression. Both drivers need source-format
list rendering. Integer uses unused local bindings to express the source sequencing and shares
the identical traversal body between its two passes. With a temporary traversal contract, its
driver through `testInteger_nofib` infers; the final `toString` lacks a list method.

Cryptarithm's shared `take` and `filter` contracts have caller checks in `preludeTests.mls`.
Their implementations time out during inference. Implicit `take`/range composition retains
an active `:fixme`, with an explicit range witness tested separately as a diagnostic control.
The benchmark-local `addj` timeout blocks permutation generation and the entry point; no trusted
signature substitutes for that benchmark computation.

GCD's source translation is complete, including the original `testGcd_nofib(40)` entry point.
The kernel and tuple-mapping stages are active; the maximum and nested-comprehension inference
reproducers exceed the test time limit. These are benchmark-local functions, so the current
library-signature fallback does not replace them with trusted declarations. Continuing active
checking of the entire driver would require addressing those inference costs or agreeing to a
broader signature fallback. The remaining producer-instantiation limitation is independently
recorded in `preludeTests.mls`.

The rigid-element fixed-point map implementation in `ctmlRecursiveLists.mls` remains a separate
checked diagnostic control. Tests of the driver's mapping stages use the manually supplied
library signature and do not establish whole-program inference or runtime equivalence.

The source `enumFromTo(a, b)` includes both endpoints and returns the empty list when `a > b`.
The explicitly typed range example in `ctmlRecursiveLists.mls` is a different, exclusive-endpoint
toy; it is not the source to copy. `max_` is partial on empty lists. The source benchmark calls
`testGcd_nofib(40)`; its MLscript runtime test expects `5021`.

## References

Paths below are relative to this directory:

- [Original benchmark](../../../mlscript-compile/nofib/gcd.mls).
- [Original cryptarithm benchmark](../../../mlscript-compile/nofib/cryptarithm1.mls) and
  [runtime reference](../../nofib/cryptarithm1.mls).
- [CSE source benchmark](../../../mlscript-compile/nofib/cse.mls) and
  [Boolean-condition tests](../ctmlBooleanConditions.mls).
- [Original SCC benchmark](../../../mlscript-compile/nofib/scc.mls) and
  [runtime reference](../../nofib/scc.mls).
- [Original LCSS benchmark](../../../mlscript-compile/nofib/lcss.mls) and
  [runtime reference](../../nofib/lcss.mls).
- [Original Integer benchmark](../../../mlscript-compile/nofib/integer.mls) and
  [runtime reference](../../nofib/integer.mls).
- [Original Life benchmark](../../../mlscript-compile/nofib/life.mls) and
  [runtime reference](../../nofib/life.mls).
- [Original Calendar benchmark](../../../mlscript-compile/nofib/calendar.mls) and
  [runtime reference](../../nofib/calendar.mls).
- [Original standard helpers](../../../mlscript-compile/nofib/NofibPrelude.mls).
- [MLscript runtime reference](../../nofib/gcd.mls).
- [Producer inference reproductions](../ctmlRecursiveProducerInference.mls) and
  [isolated subtyping reproductions](../ctmlRecursiveProducerSubtyping.mls).
- [Recursive encoding tests](../ctmlRecursiveEncoding.mls),
  [explicitly typed list controls](../ctmlRecursiveLists.mls), and
  [target-instantiation tests](../ctmlTargetInstantiation.mls).
- [Consumer inference controls](../ctmlRecursiveConsumerInference.mls) and
  [timeout investigation](../../../../main/scala/hkmc2/ctml/core/inference/recursive-consumer-performance.md).
- [Deferred producer-inference investigation](../../../../main/scala/hkmc2/ctml/core/inference/nofib-producer-inference.md).
  Its proposed solver work is not the next porting task.
- [Recursive encoding design and proof report](../../../../main/scala/hkmc2/ctml/core/subtyping/recursive-encoding.md).
  Includes proof obligations and a preexisting cyclic-constraint soundness issue; do not treat
  arbitrary subtyping cycles as proofs or assume every accepted constraint system is sound.

The reports support later checker, paper, and Lean work. They do not require that work to be
completed before adding further Nofib inference tests.
