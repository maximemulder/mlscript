# Removing conditional wrappers from the practical implementation

The normal parser already wraps class tests: CTML rebinds a variable scrutinee, and UCS supplies
a binding for a non-variable scrutinee. The relevant experiment starts from this parser output
and removes those boundaries. The [raw-AST wrapping comparison](raw-wrapping.md) is supplementary;
its per-match wrapping strategy is not the normal parser's baseline.

## Policies and safe removal

| Policy | CTML's variable wrapper | UCS's scrutinee binding |
| --- | --- | --- |
| `default` | Keep | Keep |
| `no-ctml` | Remove with `no-wrap-ifs` | Keep |
| `dewrap` | Remove with `no-wrap-ifs` | Inline where safe with `dewrap-ucs-ifs` |

`dewrap-ucs-ifs` only considers UCS bindings whose body starts by matching the bound scrutinee.
A variable, including an ascribed variable, can replace its free occurrences. An arbitrary
computation can replace the first scrutinee only when that is the binding's sole free occurrence.
Repeated tests or branch uses of a computation retain the binding and its single evaluation.
Substitution respects shadowing and declines removal if it would capture a free variable.
Ordinary source lets translated by `parseBlock` and explicit lambda applications are retained.
Boolean conditionals translated through `_if_` are outside this experiment.

The default policy is unchanged. `wrap-ucs-ifs` restores UCS binding preservation independently
of CTML's existing `wrap-ifs`/`no-wrap-ifs` switch. Removing a boundary changes inference and
generalization even when the substitution preserves evaluation; preserving evaluation alone does
not establish that inference will find the same typings.

## Differential run on actual sources

`dewrapping_bench.py` creates disposable copies of 31 existing CTML source files and runs each
policy through the normal parser, elaborator, and CTML diff-test entry point. It disables debug
traces in these copies. It excludes the prelude, the two files explicitly switching wrapping
policies (`ctmlMatchWrapping` and `ctmlSimplification`), and the wrapping/dewrapping experiments'
own regressions. Original sources and goldens are not rewritten by this comparison.

The corpus contains 599 output-producing blocks and 331 inferred-expression samples per policy.
`dewrapping-results.json` preserves sources, outputs, outcomes, settings, logs, and source digests;
the CSV contains inference samples. The outcome comparison is:

| Policy | Successful blocks | Type-error blocks | Newly accepted | Newly rejected |
| --- | ---: | ---: | ---: | ---: |
| `default` | 464 | 135 | — | — |
| `no-ctml` | 464 | 135 | 0 | 0 |
| `dewrap` | 459 | 140 | 0 | 5 |

The five newly rejected blocks have three primary causes and two cascading callers:

- `ctmlFlowCorrelation`'s `g10`, which matches `(a as Int | Str)`, fails at
  `Int | Str <= Int`. Its two subsequent callers have no inferred definition to use.
- `ctmlMatch`'s multi-arm match on literal `1` fails when checking its unreachable alternative:
  the concrete scrutinee cannot satisfy `Int <= not Int`.
- Its multi-arm match on `(1 as Int | Str)` likewise loses the flexible proxy and fails.

No previously rejected block becomes accepted in this corpus. This is a differential acceptance
check, not a proof of soundness or inferred-type equivalence. Disabling only CTML's wrappers also
changes 30 successful outputs and 15 error outputs without changing block outcomes. Further UCS
dewrapping changes 31 successful outputs and 17 error outputs, in addition to the five outcome
changes. Output changes include guard ordering as well as substantive scheme changes.

The excluded boundary tests matter: `ctmlMatchWrapping` already demonstrates that `no-wrap-ifs`
loses a direct match on a variable declared at `Int | Str`. The unchanged acceptance of this
corpus therefore does not justify removing CTML's wrappers unconditionally.

## Source-level size and timing

The focused run uses anonymous forms of seven existing suite definitions and two source-level
scaling fixtures. Every source goes through the normal pipeline; it never constructs raw CTML
expressions or inserts extra wrappers. Each fixture/policy runs alone in its own diff-test file,
with three warmup expressions followed by five measured expressions. Files run serially through
the reused sbt JVM. Inference timing excludes parsing, elaboration, type-size counting, and
formatting; these costs still occur between samples. JIT/GC and fixed policy order make small
timing differences unreliable. Corpus timings, whose files run concurrently, are diagnostic only.

Selected median inference times and final type-tree node counts from
`dewrapping-focused.csv`/`.json`:

| Source fixture | Default ms / nodes | No CTML wrapper ms / nodes | UCS removal ms / nodes |
| --- | ---: | ---: | ---: |
| `g1`: two correlated matches | 58.7 / 108 | 42.9 / 104 | 48.5 / 104 |
| `g4`: nested distinct scrutinees | 28.7 / 164 | 26.1 / 179 | 36.0 / 179 |
| `f6`: nested dead alternative | 4.5 / 13 | 5.2 / 30 | 5.7 / 30 |
| `tbad`: partial operation | 14.3 / 49 | 5.2 / 45 | 5.0 / 45 |
| `tsel`: nested partial operation | 33.5 / 193 | 31.8 / 199 | 31.8 / 199 |
| Eight sibling tests | 2082 / 227 | 428 / 227 | 405 / 227 |
| Eight nested tests | 2370 / 221 | 20.8 / 110 | 18.6 / 110 |

The source scaling fixtures confirm that removing existing wrappers can substantially reduce
inference cost. For smaller examples, it can either enlarge or shrink a scheme; an abstraction
boundary also provides an opportunity for local simplification. Equal node counts do not imply
equivalent types. The focused run's `g10` failure under UCS dewrapping is a lost typing, not a
speed improvement. Its computed-scrutinee fixture `g11` succeeds under all three policies.

These results support exploring selective wrapper removal, while retaining a flexible proxy for
concrete scrutinees and preserving useful local simplification boundaries. They do not establish
a universal replacement policy, and this experiment does not enable one by default.

## Computed-scrutinee scope

Direct computed scrutinees require the branches to see the declarations and bounds generated by
scrutinee inference. `inferMatchBranches` receives that extended context through `ctx.seq`.
Capturing the outer `ctx` while forming the branch context would omit the scrutinee's fresh
result variable and raise an unknown-variable exception. The `computed` regression checks direct
matching of `id(a)`, its honest callers and overload, and rejection of an incorrect result type.

`ctmlDewrapping.mls` also checks safe literal and computed-scrutinee removal, retention of a
multiply used computed binding, shadowing, and explicit source wrappers. Traces show the resulting
CTML expressions. Valid concrete matches that direct inference cannot find are marked `:fixme`.

## Reproduction

From the repository root:

```sh
python3 hkmc2/shared/src/main/scala/hkmc2/ctml/experiments/dewrapping_bench.py
python3 hkmc2/shared/src/main/scala/hkmc2/ctml/experiments/dewrapping_bench.py \
  --focused --output /tmp/dewrapping-focused.json
sbt --client 'dtest ctml/'
sbt --client shutdown
```

The corpus's `dewrap` run deliberately fails the original expectations in two copied files;
its JSON records these failures and their causes. A rejection, exception, or timeout must not
be counted as a faster success. The driver removes its temporary test directories afterwards.
Use `--output /tmp/dewrapping-check.json` for reruns that should preserve the recorded results.
The opt-in `profile-infer` flag supplies timing/size samples through the CTML test entry point;
normal tests do not emit them.
