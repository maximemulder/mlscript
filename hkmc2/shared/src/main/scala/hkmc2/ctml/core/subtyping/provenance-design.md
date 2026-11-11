# Provenance-aware subtyping and recursion: investigation handoff

## Decision and scope

The agreed next direction is to experiment with provenance-aware instantiation and cycle handling
as the main route toward mitigating CTML's subtyping cycles and complexity, and eventually
unifying the Scala and Lean algorithms. FCCTI+ supplies ideas for bounded instantiation and
sharing; Lean supplies an explicit account of returned obligations and terminating search.
Neither is a specification to copy unchanged.

The recursive-summary approach in `backup/fix-recursion-1` remains a useful comparison and
potential invariant-synthesis heuristic. It is not the selected foundation for the general solver.
Its successful examples are benchmarks for the provenance experiment, not permission to weaken
subtyping or discard constraints to reproduce their results.

This document records the baseline comparison and the research plan. The first implementation,
its measurements, and the remaining limitations are recorded in
[provenance-experiment.md](provenance-experiment.md). It adds conservative search cuts, not a new
soundness theorem, general termination theorem, or FCCTI+-style instance-sharing rule. The baseline
descriptions below remain useful for understanding the experiment's starting point.

## Source snapshots and reading map

Investigation date: 2026-11-11. Repository paths below are relative to this MLscript checkout
unless stated otherwise. Branch names and external working trees may change; inspect them again
before implementing.

- Scala baseline: `ctml`, commit `959504de96cba417a74a55987f7cf32188461b63`.
- Summary experiment: `backup/fix-recursion-1`, commit
  `713d0854ac64b89c24fc99ab39f60e14d3e5b257`, one commit beyond that baseline.
- Active Lean project: `../ctml/lean/CTMLCore`, in repository commit
  `2e4c25df5ca4d2cdc26b9d98e2f22a014ff790af`; its project subtree was clean when recorded.
  Use this project, not the frozen `fcct-demorgan` archive or the separate FCCT mechanization.
- FCCTI / FCCTI+ paper: `../first-class-polym/constrained-types-paper-new`, repository commit
  `4a385c8e486fc6793cf8a8a2ed7371a83eff258f`. The working tree has modifications to
  `figures/small-steps-inference-rules.tex`, `prelude.tex`, and `sections/rw-notes.tex`.
  The comparison concerns the local sources, not an assumed published version.

Start with these local reports and Scala sources:

- [Performance investigation](../inference/recursive-consumer-performance.md).
- [Recursive encoding and known negative example](recursive-encoding.md).
- [Summary proposal](../inference/recursive-summary-design.md); the backup branch contains its
  implementation report at the same path.
- [Trail](trail.scala), [subtyping rules](constraining.scala), and [splitting](split.scala).
- [Variable declarations](../../types/Clause.scala) and [freshening](../var_/decl.scala).

Under the active Lean project's `CTMLCore/Algorithmic/`, read `Trail.lean`, `Solver.lean`,
`Solve.lean`, `Soundness.lean`, `Termination.lean`, `Agreement.lean`, `Instantiation.lean`, and
`CompletenessHoles.lean`. The project notes `notes/algorithmic-function.md` and
`notes/algorithmic-system.md` explain the design; resolve discrepancies against executable rules
and theorem statements rather than assuming all prose is synchronized.

In the FCCT paper, read `sections/inference.tex`, `sections/inference-soundness.tex`,
`sections/terminating-inference.tex`, and `figures/small-steps-constraint-solving-rules.tex`.
The macros `smallsys` and `bigsys` denote FCCTI and FCCTI+, respectively.

To inspect the backup without changing the working branch:

```sh
git diff 959504de9 713d0854a -- hkmc2/shared/src/main/scala/hkmc2/ctml
git show 713d0854a:hkmc2/shared/src/main/scala/hkmc2/ctml/core/inference/summary.scala
git show 713d0854a:hkmc2/shared/src/main/scala/hkmc2/ctml/core/inference/check.scala
```

## Problems to distinguish

1. **Exact proof-search cycles:** a bound lookup returns to the same judgment and assumptions.
2. **Fresh-instantiation chains:** essentially the same computation recurs with new variables,
   so syntactic equality never recognizes it.
3. **Finite but expensive search:** repeated bound comparisons, Boolean decomposition, guarded
   context joins, and simplification can be costly even without fresh-variable growth.
4. **Recursive invariant inference:** a term-level recursive function needs a reusable contract.
5. **Constraint discharge:** returning residual constraints does not establish their satisfiability.

These problems interact, but an intervention for one is not automatically a solution to the others.
Finite abstract contexts do not alone bound the number or size of CTML's guarded constraints.
Even a finite state space can be exponential and impractical.

The performance report isolates a concrete bottleneck: nonrecursive `walkStep`, `lengthStep`,
`copyStep`, and `mapStep`, with a supplied recursive parameter, infer quickly; closing recursion
through `fix` exceeds the measured timeout. The expensive comparison feeds the inferred guarded
scheme back against `F -> F`. Samples show repeated subtyping, splitting, and semantic bound
checks. Ordinary field recursion without a constructor branch can be fast. Explicit recursive
list encodings are not necessary to reproduce the timeout.

Those measurements do not prove divergence or identify fresh instantiation as the dominant cost.
Removing selected semantic redundancy checks was insufficient in the recorded diagnostic.

## Comparison of the four solver approaches

### CTML Scala

`SubtypingTrail` is path-local: it is passed down the search, not accumulated across siblings.
It records syntactic `(sub, sup)` pairs with a variable or negated variable as a side. It does
not include the context or compare judgments up to fresh-variable renaming.

`subtypeTrail` discharges a repeat when either exposed variable is flexible; otherwise the repeat
fails and alternatives may be tried. Rigid cycles cannot prove arbitrary inclusions. For example,
`A <= B | Str` and `B <= A | Str` do not imply `A <= Str`: `A = B = Int` is a countermodel.
`ctmlCyclicBounds.mls` covers this and related cases.

A flexible bound is checked against opposite bounds **before** it is recorded. For a new
`alpha <= U`, the solver checks the existing lower bound against `U`, then returns the new bound.
The trail temporarily closes exact flexible repeats, but intermediate lookups cannot read the
entire new bound. This avoids a self-application instantiation chain that arises when the new
polymorphic bound is already available during its own check. It is not a general termination proof.

Ordinary universal opening allocates fresh flexible variables on the left and rigid variables on
the right. A separate path-local `instances` set prevents repeated target-directed witness
attempts for carrier universals; a repeated attempt is skipped, not accepted as a proof.
Splitting also restricts cyclic bound lookup inside operands to prevent continually growing goals.

`TypeVarDecl.origin` already retains an optional immediate origin during freshening and extrusion.
It does not constitute a stable provenance abstraction, and the subtyping trail does not use it.

### CTML Lean

The development distinguishes an inductive solver relation, an executable semi-algorithm, and
several trail policies. Ordinary opening likewise uses flexible variables for left universals
and rigid variables for right universals. Right-universal checking eliminates or hoists generated
variables and checks rigid-variable escape.

Unlike Scala, the flexible-variable rules assume a new bound **before** checking opposite bounds,
and include that bound in the output. Leaf rules, including hypothesis, run before the revisit
cut. A repeated assumed flexible bound may therefore close by hypothesis; the exact-repeat
failure policy does not mean every flexible cycle fails.

The terminating `Policy.cut` distinguishes:

- Exact state repeats: depth, modes, assumptions, and goal repeat; fail the lookup.
- Abstract key revisits: preserve input-variable identities, rename created variables by first
  occurrence, and read selected bounds; return the goal as a new output obligation.

The bounds help distinguish different variables of one instantiation. The documentation describes
this as using the context in place of binder provenance. Reading all bounds can prevent practical
loop recognition as old variables accumulate new bounds; reading the goal alone can confuse
unrelated lookups. `Serializer.sides` is the current compromise.

Keys additionally have bounded length and bounded symbols, fixed by the input. These clamps make
the key space finite even when goals grow. Termination is proved for the cut policy, and
`solveTotal` removes the fuel parameter. This proves termination, not a useful runtime bound.

The soundness statement is packaging soundness: the output context derives the input goal.
Deferral is sound because the goal remains an assumption. It is not a proof that the returned
constraints have a solution. Elimination substitutes a witness and re-solves the removed bounds;
checks made under those bounds cannot simply be trusted after removing them.

For self-application `Omega = (lambda x. x x) (lambda x. x x)`, the Scala golden in
`ctmlSuperF.mls` reports `Bottom`; Lean's examples show an uncut fuel exhaustion and a cut result
with cyclic bounds and deferred obligations. These outputs have different meanings.

### FCCTI

The generated bound contexts have unique lower bounds and an acyclic ordering of dependencies.
These are invariants of the restricted inference process, not properties of arbitrary CTML
constraints. Bound lookup therefore does not need CTML's general cycle policy.

Copying polymorphic bounds refreshes their bound variables. The apparently substitution-free
`Forall` rule relies on that freshening convention. Acyclic bound contexts do not prevent an
infinite succession of fresh instances: FCCTI remains a semi-algorithm tied to term reduction.

### FCCTI+

Binder/location marks survive freshening. A finite abstraction of accumulated marks indexes an
instantiation cache; the paper chooses sets of marks. `Forall1` opens and caches an instance on
a miss. `Forall2` relates incoming and cached bodies through bidirectional constraints `U` and
continues with the cached instance on a hit.

`Var1` and `Var2` record bounds and propagate them against opposite bounds, including bounds
arriving later. `Skip` avoids reprocessing recorded bounds. This is persistent constraint
propagation, rather than a path-local proof-search trail.

Sharing limits instantiation diversity and permits cyclic bounds. The paper gives a semantic
soundness argument and explicitly places the hypothetical declarative counterpart beyond pure
FCCT, with recursive types. Its argument does not transfer automatically to CTML's negation,
rigid assumptions, and guarded context disjunction. The particular abstraction controls precision;
its finite range alone is not a practical complexity guarantee.

## What the recursive-summary branch contributes

The backup introduces `ERec`, a structural candidate builder, and a body checker. It does not
modify the underlying subtyping trail, universal-opening rules, or extrusion machinery.

The builder traverses the source once and uses a finite union-find graph of shapes. Recursive
references share a node instead of recursively visiting the body. It serializes supported positive
record cycles as `R_F = forall X. {F(X) <= X} => X`. Cycles crossing arrows/products and mutually
dependent carrier binders are unsupported. External schemes receive fresh payload slots per use.

The chosen contract `H` is validated under `self : H`, corresponding to checking a generator at
`H -> H` and using the existing fixed-point contract. Inferred payload parameters are rigid during
validation, then generalized for outside callers. Recursive calls therefore share those payloads;
independent uses of the exported function remain polymorphic.

The checker preserves local immutable-path refinements, checks each reachable branch, and proves
encoded-data unfolding obligations through existing subtyping. Branch histories do not flow into
another analysis of the recursive body. Validation processes generated obligations and checks a
constrained `Unit` witness before returning the contract. Unsupported construction falls back to
ordinary inference; failed validation is a type error.

This is a principled invariant-synthesis/checking architecture with a restricted candidate
language, not an arbitrary cycle-discharge rule. It selects usable contracts rather than claiming
principal inference, general solver termination, or termination of the recursive program. Its
separate checking path and specialized candidate language make it a less suitable foundation for
Scala/Lean unification. Candidate synthesis can remain an optional layer over a common checker.

The branch reports 58 passing diff-test files, with known failures marked. That is the commit's
recorded validation, not a test run performed for this comparison. Its nested-list result
counterexample remains a solver soundness issue even with a manually supplied precise signature.

## Proposed provenance architecture

Keep three concepts distinct:

1. **Concrete identity:** the particular fresh variable. Existing scope and equality reasoning
   must continue to distinguish independent instances.
2. **Stable origin:** the binder or inference operation responsible for its creation. Specify
   preservation through copying, substitution, generalization, and extrusion. Source locations
   alone are insufficient for generated types and constraints.
3. **Abstract instantiation context:** a deliberately bounded description of relevant ancestry
   or flow, used to decide which computations recur or which instances an approximation shares.

Origin is not equality: two uses of `forall A. A -> A` share an origin but may require independent
`Int` and `Str` instances. Unbounded ancestry merely moves divergence into the provenance data.
Captured environments and rigid scopes matter even when binder origins coincide. Distinguish
creation roles, such as quantifier opening, application results, and extrusion; decide how
polarity and directional extrusion affect the abstraction instead of conflating them implicitly.

Use provenance initially as algorithmic metadata, with a clear erasure to the existing type
language. A side table or annotated internal representation is an implementation choice, not
yet a decision. Follow the repository's prohibition on adding global mutable state.

Separate the abstraction from the action taken on a collision:

- **Detect and preserve:** defer the actual obligation, or report an explicit inconclusive
  outcome where it cannot safely be packaged. An abstract match is not proof of the goal.
- **Reuse completed work:** cache results with their assumptions, residual obligations, fresh
  outputs, and scope requirements accounted for. This is distinct from an in-progress trail.
  A changed context cannot blindly reuse failures or context-dependent successes.
- **Share instances:** deliberately identify selected flexible instances and propagate every new
  bound. This is an additional approximation requiring its own soundness argument, not a
  consequence of successfully detecting a recurrence. Do not merge rigid variables by origin.

Lean's separation of serializers and revisit policies is a useful starting architecture. Explicit
provenance could replace some reconstruction from bounds; it need not reproduce Lean's current
serializer. Agreement should concern rules and observable obligations, not identical internal
representations in Scala and Lean.

## Soundness boundaries and unresolved choices

Before aggressive sharing, specify whether a solver result means a closed derivation, a derivation
under returned obligations, or an inconclusive computation. Do not conflate any of these with
existence of a satisfying instantiation. Decide how obligations are retained at generalization,
elimination, explicit checking, and top-level acceptance, including the applicable value
restriction.

The known counterexample is a mandatory negative control:

```text
N = forall X. {not X <= X} => X
N <= not N
```

The intended Boolean interpretation forces `X = Top` in the guard, so `N = Top` and the comparison
would require `Top <= Bottom`. Scala accepts it in `ctmlTargetInstantiation.mls`, marked `:breakme`.
The investigation records cyclic residual flexible bounds and acceptance with target-directed
instantiation disabled. It does not isolate the trail as the sole cause. Neither a revisit key nor
a terminating worklist resolves this discharge problem by itself.

Other open choices include the finite context abstraction, treatment of captured variables,
context-sensitive cache validity, and the representation of guarded disjunctions. A worklist or
strongly connected component strategy may help, but requires a specified monotone information
order and careful separation of assumed guards from generated obligations. Finitely many
variables do not imply finitely many CTML formulas. Sharing representations and controlling
Boolean expansion may be necessary alongside bounding instantiation.

## Staged experiment and evaluation

1. **Record a baseline and common result contract.** Run the CTML suite before refactoring and
   preserve marked failures. Inventory Scala/Lean differences in bound insertion, hypothesis
   handling, guard ordering, scoping, elimination, and returned obligations. Do not transplant a
   trail policy while leaving its supporting invariants unspecified.
2. **Add provenance and measurements without changing acceptance.** Count created variables by
   origin/context, repeated lookups, distinct bounds, formula growth, and expensive subtyping
   calls. Separate time in fresh instantiation from repeated work over existing guarded bounds.
3. **Experiment with common revisit detection.** Compare keys on the same small examples in both
   implementations. Preserve obligations or report inconclusive results at unproved cuts. Check
   that ordinary independent polymorphic uses are not confused and rigid scopes remain distinct.
4. **Test bounded sharing on an explicit fragment.** Define propagation and cyclic-constraint
   semantics, then establish soundness before widening the fragment. Add completed-result reuse
   only with a clear account of context changes and fresh output renaming.
5. **Measure generality against the summary branch.** Aim to recover useful recursive contracts
   without requiring its structural-summary path, while also improving nonrecursive higher-order
   examples. A finite runtime with unusable residual guards is not equivalent to successful
   invariant inference; report both runtime and output quality.

The first milestone is evidence for or against the provenance hypothesis, not a promised complete
solver. A useful negative finding is that the principal cost lies in guarded-bound expansion
rather than fresh instantiation. Keep metadata, cycle policy, and sharing experiments separable
so the source of improvements and precision losses can be identified.

Use at least these regression families:

- Rigid and negated cycles: `ctmlCyclicBounds.mls`, `ctmlTargetInstantiation.mls`.
- Independent instances: `ctmlPolymorphism.mls`, `ctmlImpredicative.mls`, Church examples and
  self-application in `ctmlSuperF.mls`; Lean's looping and nonlooping revisit ledger.
- Scope and witness selection: explicit instantiation, extrusion, higher-rank checking, and
  `ctmlRecursiveProducerSubtyping.mls` / `ctmlRecursiveProducerInference.mls`.
- Search cost: the minimal unary/list controls in `ctmlRecursiveConsumerInference.mls`, their
  nonrecursive step functions, and carefully isolated timeout blocks from the performance report.
- Output quality and soundness: the backup's `ctmlRecursiveSummaries.mls` and
  `ctmlRecursiveSummarySoundness.mls`, plus baseline `ctmlNestedListResult.mls`.
- Flow sensitivity: existing flow/context-join tests, heterogeneous-head refinements, invalid
  recursive arguments, unused failing initializers, and captured-variable relationships.

Do not equate different Scala/Lean packaging of residual constraints without checking their
meaning. Compare accept/reject/inconclusive behavior, retained obligations, useful inferred
contracts, and resource use. Include supported-fragment boundaries in any differential comparison.

Follow `AGENTS.md`: CTML-only code changes require the CTML suite, review rewritten golden output,
and stop the sbt server afterward. Files exceeding 30 seconds need an isolated failing block and
a fix or documented FIXME exclusion; target less than five seconds per file. Read the Nofib porting
guide before changing ports. Do not rewrite examples to conceal an unresolved solver issue.

## Status of this handoff

The comparison and staged plan are the baseline for the implemented
[provenance experiment](provenance-experiment.md). Stable origins, measurements, conservative
revisit detection, and independent query budgets are implemented in Scala. Instance sharing and
Lean unification remain open. In particular, the measured recursive-consumer bottleneck is mostly
optional checking over existing guarded bounds, and useful inferred list contracts remain missing.
