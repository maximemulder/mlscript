# Supplementary raw-AST wrapping experiment

This comparison starts from manually constructed, unwrapped CTML terms. It does not measure
removal of the wrappers inserted by the normal parser. See [the source-level reverse
experiment](README.md) for that comparison and its conclusions about the current implementation.

This is an isolated executable experiment, inspired by
`../ctml/lean/CTMLCore/notes/wrapping.md` and the paper's section on wrapping a conditional.
It leaves default parsing, inference, join construction, and simplification unchanged.

Unlike the Lean experiment, ordinary inference on unused-parameter wrappers preserves every
successful baseline fixture in this matrix. Wrapping sometimes reduces nested schemes, but does
not give a general speedup. Scrutinee rebinding solves an additional inference problem and can
also be much more expensive than an unused wrapper.

## Strategies

`Wrapping.scala` constructs CTML expressions directly and recursively rewrites each match:

| Strategy | Input at a match `e`, with variable scrutinee `x` |
| --- | --- |
| `bubble` | `e` |
| `dummy` | `(lambda unused. e) unit`, with a fresh unused binder |
| `rebind` | `(lambda x. e) x` |

Here `bubble` names the executable baseline: `inferMatch` joins branch bounds immediately and
returns those bounds through its continuation to an enclosing inference level. This executable
does not construct the Lean soundness proof's tree of alternative contexts. The comparison
measures the effect of adding inference levels and applications, not the cost of proof bubbling.

All inserted lambdas go through ordinary inference, including `processLevel`'s solving,
simplification, packaging, and quantification. This is distinct from the Lean **typed output**
wrapping construction, which leaves inference answers unchanged by construction. No operational
simulation or soundness theorem for these Scala transformations is claimed here.

The existing parser's `wrap-ifs` option implements a related scrutinee rebinding, enabled by
default. The benchmark bypasses the parser so its bindings cannot contaminate the baseline.
`rebind` rewrites **every** raw match, including exhaustive matches; the parser instead shares a
binding across a UCS split and keeps UCS's bindings even with `no-wrap-ifs`. Its timings therefore
describe the explicit transformation, not the default parser's exact behavior.

## Fixtures and acceptance

There are 37 fixtures: flow functions and their callers, a partial operation on a second
argument, nested tests with an explicit absurd branch, the practical `foo`/`foo2` exhaustive
splits, let-bound polymorphic use, concrete union matching, binder capture, and sibling/nested
families of sizes 1, 2, 4, and 8. Nested-family tests reuse one scrutinee; the two-argument `sel`
fixture also tests distinct scrutinees. Calls inline their function except `let-polymorphic`.

The partial operation is `b as Int`, since this CTML expression language has no record selection
to reproduce Lean's `b.f`. Practical exhaustive matches use `EMatch(..., None)` rather than Lean's
explicit absurd final branch. These differences and the practical simplifier make absolute
cross-implementation type-node counts and timings incomparable.

Each successful caller is checked against its expected result type. The six negative fixtures
exercise an incorrect result annotation, incompatible partial-operation arguments, an absurd
branch, a missing exhaustive case, mismatched correlated arguments, and two independent union
arguments. In the last case, distinct Int and Str values are each widened to Int | Str.

The matrix has 111 cells and 777 samples. Every cell completed all seven repetitions; outcomes
and node counts are stable within each cell. There are no crashes, timeouts, or resource-limit
outcomes. `bubble` and `dummy` each accept 30 fixtures and reject 7; `rebind` accepts 31 and
rejects 6. All strategies reject all six negative fixtures. Their only acceptance difference is
the valid `concrete-union` fixture, rejected by `bubble` and `dummy` and accepted by `rebind`.

An unused lambda leaves the concrete scrutinee's union type visible to the branch constraint
`Int | Str <= Int`, which fails before a join can be formed. Rebinding supplies a fresh flexible
parameter whose branch bounds can be joined before the actual union argument is supplied.
The regression marks dummy wrapping's missing inference with `:fixme`.

## Scheme size and cost

Selected final type-tree node counts, including types in constraints:

| Fixture | `bubble` | `dummy` | `rebind` |
| --- | ---: | ---: | ---: |
| `flow-infer` | 31 | 31 | 31 |
| `bad-infer` | 45 | 49 | 49 |
| `sel-infer` | 84 | 49 | 49 |
| `foo-infer` | 31 | 31 | 31 |
| `foo2-infer` | 67 | 67 | 67 |
| `siblings-8` | 229 | 229 | 229 |
| `nested-8` | 110 | 80 | 221 |

The simple flow schemes retain their alternative guards, unlike the Lean probe's simultaneous
class obligations. Identical node counts alone do not establish type equivalence. The schemes
and error trees are preserved in the JSON files for inspection; regression tests separately check
both honest callers and the overload intersection.

Local level processing can simplify an inner match before its bounds enter an enclosing join.
For `sel-infer`, unused wrapping produces the same displayed scheme as the simpler `bad-infer`,
eliminating much of the nested guard structure. Conversely, `bad-infer` retains an additional
quantified result variable when wrapped. Rebinding also adds flexible scrutinee variables; the
`nested-8` result shows that their cost can survive simplification. These are observations about
the current simplification/solving policy, not equivalence or complexity theorems.

The focused timing confirmation reverses strategy order and uses more warmup; its raw data are
in `wrapping-confirmation.csv` and `.json`. Median inference times in milliseconds:

| Fixture | `bubble` | `dummy` | `rebind` |
| --- | ---: | ---: | ---: |
| `sel-infer` | 7.1 | 17.1 | 9.5 |
| `siblings-8` | 404 | 770 | 1974 |
| `nested-8` | 20.2 | 17.9 | 1986 |

JVM timings remain approximate. Scheme reduction does not imply an inference speedup, and none
of these measurements justifies changing the default policy. In particular, the parser's
rebinding has a completeness role that dummy wrapping alone cannot replace. A promising
follow-up is selective rebinding when the scrutinee needs a flexible proxy, combined with local
simplification where nested joins grow; that policy is not implemented by this experiment.

## Reproduction

From the repository root:

```sh
sbt --client 'export hkmc2JVM/Runtime/fullClasspath' > /tmp/ctml-wrapping-classpath.log
python3 hkmc2/shared/src/main/scala/hkmc2/ctml/experiments/wrapping_bench.py \
  --classpath-file /tmp/ctml-wrapping-classpath.log
sbt --client 'dtest ctml/'
sbt --client shutdown
```

The driver needs only Python's standard library and the compiled JVM classpath. Each cell runs
in a fresh JVM with `-Xss8m -Xms128m -Xmx1g`, five warmup inferences, and seven measured inferences.
It kills and waits for a process exceeding 30 seconds, including startup and warmup, before
starting another cell. This deadline is distinct from a type rejection. Debugging is disabled;
`checkUnsolvableConstreds` is enabled, as in the practical flow tests. Every sample resets the
configuration and fresh-variable counter. Top-level inference packages all output bounds into
the result type, so successful samples assert that no output clauses remain.

Measured time includes configuration/counter reset, inference, and type-node counting. It excludes
rewriting, constructing the primitive context, JVM startup, and formatting/emitting type/error
text. It does not isolate the solver from simplification or generation. Samples in one JVM are
not independent measurements of an optimized steady state; small timing differences are noisy.

For the confirmation, add `--variants rebind dummy bubble --cases sel-infer siblings-8 nested-8
--warmups 10 --repeats 10 --timeout 60 --output /tmp/wrapping-confirmation.csv` to the Python
command. Use `--cases` and `--output /tmp/wrapping-check.csv` for other exploratory reruns.
The JSON records the base revision, JVM, settings, source digests, and one type/error per cell.

The initial baseline passed all 34 CTML files without golden changes. The new regression is
`hkmc2/shared/src/test/mlscript/ctml/ctmlJoinWrapping.mls`; it uses `no-wrap-ifs` and explicit
wrappers so the parser cannot hide the distinction being tested.
