# Provenance search experiment

This is the implementation report for the direction in
[provenance-design.md](provenance-design.md). The comparison in that document describes the
baseline, commit `959504de96cba417a74a55987f7cf32188461b63`. These experiments were run on
2026-11-11. No changes from `backup/fix-recursion-1` are imported. The Lean project is unchanged.

## Result

Scala retains stable binder provenance and uses it to recognize some searches that recur with
fresh variables. The default policy reports an **inconclusive** result on an abstract revisit.
It does not discharge that judgment, identify variables, or package an unchecked residual as a
successful result. Exact flexible repeats retain their separate pending-bound rule.

The implementation also provides observation mode, deterministic measurements, and independent
resource limits. Resource limits are disabled by default. These are useful for distinguishing
fresh-instantiation chains from costly search over a fixed collection of variables.

The two standalone looping probes terminate with the provenance policy alone. The recursive
list-consumer problem is **not solved**. Measurements show that the sampled closure searches
spend almost all their solver calls in optional checking queries, without repeatedly creating
instances of the same binder. Limiting those queries can produce a scheme for `walk` and `copy`,
but valid nonempty-list calls still fail. Finishing those definitions is not a usable inference
result, and the tests retain these failures explicitly.

## Representation and invariants

See [Var.scala](../../types/Var.scala), [decl.scala](../var_/decl.scala),
[provenance.scala](provenance.scala), and [search.scala](search.scala).

Concrete type-variable equality and hashing still depend only on the concrete name. Provenance
is immutable metadata erased by that equality; it is not an additional type constructor. Fresh
instances always receive fresh concrete names. This preserves the baseline's treatment of named
variables; it does not repair its separate name-based shadowing limitations.

Each variable carries:

- A root: the elaborator symbol of a source quantifier, or the name of an inference/declaration
  variable. Two source binders spelled `A` therefore have different roots. Source identities are
  stable within the elaborated program, not persistent identifiers across unrelated parses.
- A creation role: root, flexible/rigid opening, or positive/negative extrusion. This is distinct
  from the declaration's current kind: rigidifying a query does not change its variable's origin.
- An abstract flow context: the set of binder roots opened along the path, combined with the
  original variable's context and its own root. Set union is idempotent; ancestry is not a linked
  chain of fresh variable identities.

Opening a generalized instance preserves its root. Capture-avoiding substitution preserves the
renamed binder's provenance. Extrusion preserves the root and flow context while recording its
direction. Display renaming preserves metadata. The immediate `TypeVarDecl.origin` remains useful
for debugging and is distinct from the stable root on `TypeVar`.

There is no new global mutable state. The existing fresh-name counter supplies concrete names.
Mutable measurement counters and optional-query budgets belong to an operation and its descendants;
the types, assumptions, and path-local revisit sets remain immutable.

## Search key and cut policy

At entry to each subtyping search, the variables already declared in its input context become
anchors. This entry is **not** the start of the enclosing expression's inference session:
instantiations created while inferring arguments are already independent inputs when an argument
is checked. Anchoring at expression entry incorrectly conflates list-constructor instances.
The constructor and encoded-list regressions exercise this distinction.

Keys are computed for judgments with a variable or negated variable as a whole side. They contain
the two goals and the direct lower/upper bounds of their exposed variables. Serialization:

- Keeps input variables and all currently rigid variables at concrete identity.
- Numbers other instances by first occurrence, recording their root, role, and flow context.
  Two variables with one origin still get different numbers within a key; their aliasing pattern
  is preserved across the goals and side bounds.
- Uses lexical indices for bound occurrences and stable marks for quantifier binders.
- Distinguishes type constructors, classes, fields, constraint direction, and negation.
- Bounds depth at eight and visits at most 128 nodes per goal and 64 per side bound, with explicit
  truncation tokens. Bounds are read once rather than recursively expanded.

The depth limit matters independently of the node limit. A repeatedly doubled arrow can have an
exponentially large syntax tree while its leftmost branch is still relatively shallow. A node-only
prefix followed that branch too far, allowing ordinary type hashing to dominate before a revisit
was detected. The chosen limits are conservative search heuristics, not semantic equivalences.

An exact repeat is handled by the existing exact-trail rule. Otherwise immediate atom/hypothesis
proofs are allowed before the abstract cut. An abstract collision raises `SearchInconclusive`.
The observation policy counts the same collision but continues the original rules.

Alternative branches can still succeed after a local cut. If no alternative succeeds and at least
one was inconclusive, the result remains inconclusive. The same rule applies to the target-witness
heuristic: cutting that heuristic leaves ordinary universal instantiation available. An exhausted
whole-operation budget propagates directly rather than being caught to explore more alternatives.

These keys are **not valid cache keys for completed proofs**: they omit assumptions, truncate
syntax, and abstract newly created flexible identities. They may collide on nonloops. Keeping
rigid identities also means the key space is not proved finite for arbitrary search. There is no
general termination theorem or wall-clock bound; expensive helper operations and type hashing are
not individually metered by solver-call limits.

## Optional checking and result contract

`checkSubtypeResult` distinguishes `Proven`, `NotProved`, and `Inconclusive`. `NotProved` means
the existing solver found no proof, not semantic falsity in the general type language.
The existing Boolean `checkSubtype` is a proof-availability query: only `Proven` returns true.
Thus a stopped simplification keeps the unsimplified type or asserted constraint.

Two uses of failed queries require explicit care:

1. The `ex-falso` option may refute a guard only on the existing decided Boolean fragment and a
   completed `NotProved` result. An inconclusive query cannot establish an inconsistent guard.
2. Class case-split selection does not split again on the basis of an inconclusive query. The
   type may already be in one case; repeatedly trying to split it could reproduce the same search.

Nested optional queries share their outer query's call budget. All queries in an inference share
the operation counter, including simplification after body inference. The budgets count solver
entries rather than elapsed time. The diagnostics use the existing typing-error display, but say
`Subtyping search inconclusive` explicitly; no closed proof or residual solution is returned.

The pre-existing success contract for `SubClauses` is unchanged. In particular, this work does not
prove satisfiability of cyclic flexible bounds or repair the known acceptance of
`N <= not N`, where `N = forall X. {not X <= X} => X`. That `:breakme` remains in
`ctmlTargetInstantiation.mls`. The nested-list result counterexample also remains marked.

## Controls and measurements

Use `:ctml-cfg` with these independent controls:

| Option | Meaning |
| --- | --- |
| `provenance-cut` | Detect abstract revisits and report inconclusive; default |
| `provenance-observe` | Count abstract revisits without cutting them |
| `no-provenance` | Disable abstract keys; retain concrete identity and exact-trail behavior |
| `profile-search` | Report deterministic per-operation measurements |
| `search-steps N` | Stop the operation after `N` solver calls |
| `check-steps N` | Limit each outer optional checking query, including its nested queries |
| `no-search-limit` | Disable the operation limit; default |
| `no-check-limit` | Disable the optional-query limit; default |

Limits require a positive integer. They also work with `no-provenance`, so an improvement caused
by resource scheduling can be separated from one caused by provenance recognition.

`@ctml-search` reports solver calls, calls in checking queries, fresh openings/extrusions, inference
variables, distinct roots and provenance groups among openings/extrusions, maximum allocations per
group, exact/abstract repeats, local cut events, largest context clause count, and largest goal
node count. The goal count saturates at 4096; it is not a transitive size of the bounds graph.
Cut events include repeated nested entries into an exhausted query, not just distinct queries.
`profile-infer` separately reports time and inferred type size, including an `inconclusive` status.
Timing output is excluded from regression goldens.

## Evidence

The baseline passed all 56 CTML files, including explicitly marked known failures. With provenance
disabled, the metadata experiment reproduced the existing golden outputs. Enabling cuts after
correctly anchoring each subtyping entry preserved the existing acceptance outcomes. One existing
negative target-instantiation example reports an earlier inconclusive result in place of the
eventual rigid-cycle error; this is not a new proof or an inconsistency diagnosis.

### Standalone loops and independent instances

The active examples are in `ctmlProvenance.mls`:

- Under `F <= forall X. {F <= X -> X} => (X -> X)`, the search for `F <= Int` stops after eight
  solver entries and two openings of the same binder. It uses no resource budget.
- With carrier body `X` instead, the target witness substitutes increasingly large arrows.
  Depth-bounded keys stop that path, and ordinary-instantiation fallbacks are explored as well.
  The final search stops after 322 entries, with 80 fresh variables across those fallback paths,
  all from one provenance group. It also uses no resource budget.
- Independent calls of one identity function still produce `Int` and `Str`. A three-cell encoded
  chain is accepted, a malformed tail is rejected, and a higher-rank rigid mismatch is rejected.

The focused Scala suite also checks alpha-renaming, generalization, extrusion direction, source
marks, aliasing patterns, changed side bounds, input anchoring, operation isolation, observation,
alternative success, global exhaustion, target-witness fallback, and the `ex-falso` boundary.

### Recursive consumers

The measurement is closure of the already inferred `step` function through `fix(step)`. Each
operation has its own counter, so the following figures exclude inference of the step function.
Observation mode with a 20,000-call operation limit and no optional-query limit gives:

| Closure | Total calls at stop | Checking calls | Fresh instances | Abstract revisits |
| --- | ---: | ---: | ---: | ---: |
| `walkStep` | 20,001 | 19,967 | 5 | 0 |
| `lengthStep` | 20,001 | 19,969 | 4 | 0 |
| `copyStep` | 20,001 | 19,963 | 7 | 0 |
| `mapStep` | 20,001 | 19,918 | 9 | 0 |

Every fresh instance in these four bounded prefixes has a different provenance group. This is
evidence about the measured prefixes, not proof that later search would never instantiate again.
The nonrecursive steps finish; `mapStep` alone uses 50,726 calls, of which 50,445 are checking
calls, so its measurement requires disabling the 20,000-call operation limit while preparing it.

With cuts enabled, a 128-call optional-query budget, and the same operation limit:

| Closure | Calls | Checking calls | Result |
| --- | ---: | ---: | --- |
| `walkStep` | 4,324 | 4,287 | 245-node scheme; valid nonempty-list call fails |
| `lengthStep` | 20,001 | 19,686 | Inconclusive; goal size grows to 2,191 nodes |
| `copyStep` | 5,583 | 5,542 | 441-node scheme; valid nonempty-list call fails |

Disabling provenance cuts gives the same call counts and schemes for `walk` and `copy`; both have
zero abstract revisits. Their improvement is caused by limiting optional checking. `length` still
hits the operation limit either way. The larger unsimplified guards also show why aggressively
cutting simplification is not a general cure for formula growth.

`ctmlProvenanceConsumers.mls` retains the bounded examples, their valid-call `:fixme`s, and
malformed-tail rejection. The larger observation/`map` ablation is documented here rather than
left as an additional slow regression file. It can be reproduced from the step functions in
`ctmlRecursiveConsumerInference.mls` using the controls above.

## Relation to Lean and next work

The directly transferable part is the separation of concrete identity, origin metadata, key
abstraction, and revisit action. Lean can attach stable binder marks to annotated syntax or a
corresponding provenance environment, with erasure/renaming invariants, rather than reconstructing
origins only from selected bounds. Solver-input anchoring and within-key aliasing are essential.

The policies still differ: Lean's abstract cut can return the actual goal as a conditional output
obligation, whereas Scala reports an inconclusive search. This experiment deliberately does not
translate a deferred Lean obligation into Scala success. Bound insertion, scope elimination,
packaging, and satisfiability remain differences to reconcile before claiming agreement.

The next performance experiment should address repeated optional queries and guarded-bound
propagation with a precise information order. A cache needs dependencies on assumptions and
pending proof hypotheses; the provenance key alone cannot safely cache successes. A finite graph
of instances also does not bound Boolean/guarded formula expansion.

FCCTI+-style sharing remains a separate step: define which flexible witnesses may share, account
for captured variables and scopes, and propagate every later bound. Validate that step against
the cyclic-flexible negative examples before broadening it. The current consumer measurements
give no evidence that merely limiting instance diversity will infer useful list contracts.
The backup branch's invariant synthesis/checking architecture remains relevant to that separate
problem, without becoming the general subtyping-cycle policy.

## Validation commands

```sh
sbt --client "dtest ctml/"
sbt --client "hkmc2JVM/testOnly hkmc2.ctml.ProvenanceTest"
sbt --client shutdown
```

Only CTML code and tests are changed. Review rewritten golden outputs as well as test status.

Final validation: all 58 diff-test files pass with the default cut policy, and all 23 focused Scala
tests pass. All diff-test files finish in less than five seconds in this run. The 58-file run with
the default policy temporarily disabled also passes, with no changes to the 56 original golden
files. The default cut run changes only the circular negative diagnostic described above among
those original files. Known `:fixme` and `:breakme` cases remain marked.
