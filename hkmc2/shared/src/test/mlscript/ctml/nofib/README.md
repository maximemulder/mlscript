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

Do not add parameter/result signatures, explicit type arguments, or a typed replacement for a
helper merely to make the benchmark pass. Such annotations can be useful in separate diagnostic
controls, where they establish that a failure concerns inference. Keep those controls distinct
from the actual port. Result assertions and negative tests may check the behavior of inferred
definitions without replacing them with annotated definitions.

Preserve source behavior, including range endpoints, empty cases, partial functions, evaluation
structure, and benchmark inputs. Document necessary representation changes. Do not replace a
list algorithm with arithmetic or another algorithm to avoid an inference limitation. CTML diff
tests check typing; a passing type test does not establish runtime equivalence.

The agent may choose the order of files and carry out routine translation within agreed decisions.
Report new feature-versus-rewrite decisions and substantial checker design changes to the user
before implementing them. An inference failure alone does not require such a design change.

## Agreed choices

- Port one file at a time, starting with `gcd.mls`, and add shared support as it is needed.
- Keep direct recursion. Mutual recursion and richer binding support are deliberately out of
  scope for now; revisit them only when a concrete source program warrants a decision.
- Represent triples as nested pairs, `[a, [b, c]]`, and destructure them with nested tuple
  patterns. CTML supports binary tuple patterns without spreads in `if ... is ... then` matches;
  direct parameter and `let` patterns remain outside this port's scope. Precisely typed trusted
  `fst` and `snd` primitives are also available in the prelude.
- Tuple-pattern fallthrough can expose missing product-disjointness subtyping. The focused
  `ctmlTuplePatterns.mls` tests retain these cases as `:fixme`; unconditional destructuring in
  the GCD kernel does not require these judgments.
- Use readable constructor patterns for lists. An abstract list API with trusted `caseList`
  callbacks is not the chosen porting approach. CTML supports nonrecursive constructor
  declarations and constructor patterns; the list tail's recursive relationship is a type-level
  question rather than a recursive constructor declaration.
- Explore recursive lists through the existing constrained polymorphic encoding. The encoding
  and explicit-instantiation support are implemented; native recursive types are not a
  prerequisite for continuing the port. Generic source-level alias syntax or other missing
  conveniences still need a decision if a port requires them.
- Defer automatic recursive-witness selection, consumer inference precision, and inference
  performance investigations. Record their failures while continuing the source translation.

## File-by-file workflow

1. Read the original benchmark and the helpers it uses. Identify its input, expected behavior,
   and dependencies before translating. Copy only the selected benchmark and needed helpers.
2. Translate into this directory using existing CTML constructs and approved rewrites. Record
   the original source path and representation choices in the file. Share reusable definitions
   through `NofibPrelude.mls`; CTML's `:import` already works.
3. Keep ordinary helpers and benchmark functions unannotated. Classify each obstacle using the
   table above. For an inference failure, retain a small reproducer when useful, but do not let
   an open-ended solver investigation block the port.
4. Mark valid programs rejected by the checker with `:fixme // FIXME: <reason>`. Mark invalid
   programs accepted by it with `:breakme // FIXME: <reason>` followed by `:te`. These commands
   apply to diff-test blocks, so separate successful definitions from failing uses where useful.
5. For a timeout, isolate and comment out the offending block with a `FIXME` explaining the
   limitation. Retain the source so the test can be enabled later. If dependent code cannot be
   checked, preserve it with an explicit explanation; do not introduce trusted signatures for
   missing inferred definitions to manufacture a passing benchmark.
6. Run focused tests, review the generated `//│` output, then run the CTML suite for executable
   changes. Follow `AGENTS.md` for baseline comparisons and any shared-code changes. Aim for
   under five seconds per file; exclude known timeout cases from active execution.
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
| [gcd.mls](gcd.mls) | Partial translation: extended Euclidean kernel `g` uses nested tuple patterns; `g` and `gcdE` infer successfully. Result checks and negative tests are present. The list-based driver is not translated. |
| [NofibPrelude.mls](NofibPrelude.mls) | Trusted pair projections, integer multiplication and quotient; inferred `intRem` and `quotRem`. No shared list definitions yet. |
| Unannotated `enumFromTo` | Translated in a diagnostic test and infers a recursive constrained scheme. Its implicit uses expose subtyping failures. Not yet incorporated into the Nofib prelude. |
| List consumers | Diagnostic `firstOrZero` composes imprecisely with the inferred producer. Unannotated `length` inference times out and is retained as a commented reproducer. These are deferred inference issues. |

**Next task: finish translating the GCD helpers and driver under this policy.** Read the source
prelude and bring in the list constructors, inclusive `enumFromTo`, `map`, comparison, and `abs`
as needed. Translate `max_`, `test` (including its nested list-comprehension helpers),
`testGcd_nofib`, and `main`. Preserve their unannotated definitions and record inference failures
instead of fixing the solver first. Report any genuinely new language or representation decision.

The source `enumFromTo(a, b)` includes both endpoints and returns the empty list when `a > b`.
The explicitly typed range example in `ctmlRecursiveLists.mls` is a different, exclusive-endpoint
toy; it is not the source to copy. `max_` is partial on empty lists. The source benchmark calls
`testGcd_nofib(40)`; its MLscript runtime test expects `5021`.

## References

Paths below are relative to this directory:

- [Original benchmark](../../../mlscript-compile/nofib/gcd.mls).
- [Original standard helpers](../../../mlscript-compile/nofib/NofibPrelude.mls).
- [MLscript runtime reference](../../nofib/gcd.mls).
- [Producer inference reproductions](../ctmlRecursiveProducerInference.mls) and
  [isolated subtyping reproductions](../ctmlRecursiveProducerSubtyping.mls).
- [Recursive encoding tests](../ctmlRecursiveEncoding.mls),
  [explicitly typed list controls](../ctmlRecursiveLists.mls), and
  [target-instantiation tests](../ctmlTargetInstantiation.mls).
- [Deferred producer-inference investigation](../../../../main/scala/hkmc2/ctml/core/inference/nofib-producer-inference.md).
  Its proposed solver work is not the next porting task.
- [Recursive encoding design and proof report](../../../../main/scala/hkmc2/ctml/core/subtyping/recursive-encoding.md).
  Includes proof obligations and a preexisting cyclic-constraint soundness issue; do not treat
  arbitrary subtyping cycles as proofs or assume every accepted constraint system is sound.

The reports support later checker, paper, and Lean work. They do not require that work to be
completed before adding further Nofib inference tests.
