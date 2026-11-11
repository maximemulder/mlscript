# Recursive summaries with local branch refinement

## Implemented scope

Direct recursive closures use a finite structural candidate followed by checking of the original
body. Recursive calls share the candidate contract; local constructor and payload tests retain
branch refinement. This is a restricted inference strategy, not an extension of subtyping or an
unchecked library declaration.

Unannotated `walk`, `length`, `copy`, `map`, unary traversal, and a branching-tree consumer have
usable inferred contracts. The Nofib source implementations of `map`, `take`, and `filter` replace
their trusted declarations. The partial `max_` and nested-recursive `addj` and `permutations` definitions are active.
Their algorithms and parameter annotations are unchanged. Fully annotated direct recursive
functions also use the checking path.

Unsupported candidates fall back to ordinary fixed-point inference, which may still time out.
General termination and principality are not claimed. In particular, the GCD Cartesian-product
wrapper remains outside the successful fragment, and implicit recursive-witness selection through
ordinary wrappers remains incomplete. Cryptarithm's translated driver reaches the independent
`take`/range witness failure and is an active `:fixme`; its entry point remains disabled.

## Construction: a finite graph, not recursive branch expansion

`parser/binding.scala` retains a recursive closure as `ERec`, including the resolved binding
identity. Unsupported shapes lower to the same synthetic prelude `fix` reference as ordinary
fixed-point inference; a source binding named `fix` cannot capture it.

`summary.scala` collects candidate equations using a local union-find graph. Nodes contain tagged
record alternatives, field edges, an arrow or product shape, or an opaque surrounding type. A
source parameter, result, field position, or copied signature component allocates a node. A
recursive reference reuses a node; its call does not visit the body again. Nested recursive
closures have their own nodes and retain lexical captures.

Applications merge argument/result shapes. Pattern chains without a default contribute the
constructor alternatives required by the domain. A test with an unconditional default instead
collects its positive branch with a separate shape: testing a head for `Int` does not require all
heads to be integers. These equations can overconstrain or otherwise propose an unsuitable
contract; they are not evidence that the body is typable.

Simple polymorphic external signatures are copied with fresh payload slots. The collector also
recognizes the exact one-guard encoding `forall X. {F(X) <= X} => X`, making its carrier a graph
node rather than unfolding it repeatedly. Other constrained signatures are unsupported.

Positive record cycles are serialized using the existing encoding. Independent nested cycles
are allowed, as needed for nonempty-list domains and lists of lists. Cycles crossing arrows or
products, and cycles requiring mutually dependent quantified carriers, are rejected as candidates.
The finite graph and syntax traversal bound candidate construction; this does not bound the
subsequent general subtyping checks.

Payload occurrences are tracked in the structural graph before encoding. Payloads used only as
inputs are widened to `Top`; payloads connecting input and output positions are retained and
quantified. Thus `length` accepts heterogeneous lists, while `copy` and `map` retain their element
relationships. With `List[A]` standing for the encoding, typical candidates are:

```text
length : List[Top] -> Int
copy   : forall A. List[A] -> List[A]
map    : forall A, B. (A -> B) -> List[A] -> List[B]
max_   : (LCons & {head: Int} & {tail: List[Int]}) -> Int
addj   : forall A. A -> List[A] -> List[List[A]]
```

The selected least fixed point is a deliberate usable domain, not an equivalence between an
arbitrary upper-bound constraint `X <= F(X)` and that encoding. No name such as `List`, `head`,
`tail`, or `map` has special treatment.

## Validation and soundness boundary

`check.scala` checks a proposed contract `H` under `self : H`, establishing the body obligation
corresponding to a generator of type `H -> H`. The existing fixed-point contract then justifies
recursion. Payload variables are rigid throughout this check and generalized only afterward.
No rule discharges arbitrary subtype cycles.

Expected arrows flow into lambda bodies. Local bindings evaluate/check their initializers even
when unused. Known function arrows check their arguments; generic instantiation retains its
guards. Source parameter and result annotations remain obligations.

Matches check the scrutinee, then visit each reachable branch with its type intersected with the
pattern or its negation. A missing final alternative requires exhaustiveness. Refinements are
stored on immutable variable/projection paths, with local aliases sharing their source path.
Branch refinements do not become assumptions at a recursive call or escape to subsequent code.
Ascriptions are always checked, even when their underlying path has a cached refinement.

Encoded data is exposed only as far as a pattern inspects it. Each carrier unfolding is proved
using existing subtyping; reconstruction of larger covariant record types preserves that
inclusion. Negative occurrences are not unfolded to derive stronger assumptions. This finite,
pattern-directed exposure handles the nested singleton/non-singleton patterns of `max_` without
unbounded unfolding.

Every evaluated operation, argument, branch result, and recursive argument contributes its
obligations. Generated existential constraints are processed at a checking-local level using a
`Unit` witness, and that constrained witness must check as `Unit` with enclosing variables
rigidified. This separates discharge of body obligations from rediscovery of the recursive result
witnesses already present in `H`. Captured-variable obligations cannot be silently satisfied by
strengthening the caller's context and then discarding those constraints.

Explicitly instantiated applications also use the checker when a known arrow domain is supplied
for a function argument. This supports the existing `fix[H](...)` diagnostic controls. General
infer-then-ascribe checking of an already inferred guarded scheme remains incomplete.

This describes the intended proof reduction to existing typing/subtyping rules. It is not a
machine-checked soundness proof of the implementation; existing solver soundness limitations
still apply, including the nested-result issue below.

## Preserving useful contracts

A validated recursive binding returns its scoped contract directly, without hoisting its result
carrier again at the definition boundary. Otherwise a result of type `List[A]` becomes a fresh
outer carrier whose implicit recursive witness the general solver may fail to rediscover.
Fallback recursive closures are generalized before their continuation, which also preserves
independent polymorphic uses of a local recursive identity.

Ordinary surrounding inference can still hoist a wrapper's result. The `localCopy` regression
retains this separate limitation as `:fixme`, with a successful explicit-witness control.

The simplifier conservatively preserves variables related to a nested quantified carrier in both
its inlining and reversed-bound elimination paths. Its direction-only guard traversal does not
fully account for the enclosing variance of that carrier. Erasing a shared payload can reduce
`forall A. List[A] -> List[A]` to an unusably narrow empty-list contract. Preserving these variables
avoids that loss; it does not claim a complete new polarity analysis.

## Known limitations and adversarial checks

The tests distinguish successful inference from expected failures:

- `ctmlRecursiveSummaries.mls`: unannotated consumers, callback/result relationships, mixed
  payloads, unary constructors, branching trees, and malformed tails. A wrapper-instantiation
  limitation remains explicitly marked.
- `ctmlRecursiveSummarySoundness.mls`: supplied-domain exhaustiveness, invalid recursive
  arguments, unused failing initializers, escaping refinements, invalid result annotations,
  checked ascriptions on refined paths, and captured outer-variable relationships.
- `ctmlRecursiveSummaryChecking.mls`: the nested heterogeneous-head control checks; post-hoc
  summary ascription and unsummarized helper composition remain `:fixme` cases.
- Nofib callers exercise the inferred source helpers and preserve the separate range-witness
  failures. Empty input is rejected for `max_`, preserving source partiality.

A new adversarial caller of `addj` exposes an existing nested-list result bug: inserting a string
into an integer list can be incorrectly accepted as a list of integer lists. The independent
`ctmlNestedListResult.mls` uses a manually declared precise contract, with no recursive
implementation inference. It reproduces the same acceptance with every checker source restored
temporarily to unchanged HEAD; the implementation edits were restored after that run. Both
callers are marked `:breakme`, while a concrete malformed nested list is correctly rejected.
This is an unresolved solver soundness issue, not successful negative-test coverage.

## Comparison with FCCTI+

FCCTI+'s `sections/terminating-inference.tex` shares polymorphic instantiations using a finite
abstraction of call-location marks; its `cond` also drops correlation with the Boolean condition.
The local source is `/media/Data/HKUST/Research/first-class-polym/constrained-types-paper-new/`.
Its finite constraint-space argument does not transfer directly to CTML's guarded disjunctions.

This implementation instead restricts the inferred recursive contract and validates the body
against it. It retains local flow refinement, but does not carry branch histories across recursive
calls. It leaves ordinary nonrecursive context-disjunctive inference and the subtyping rules in
place. Instantiation sharing and completed-judgment caching remain complementary experiments.

`:ctml-cfg no-recursive-summaries` selects ordinary fixed-point inference for recursive bindings;
`:ctml-cfg recursive-summaries` reenables summaries. The explicitly instantiated argument checker
and conservative simplifier protection are independent of this switch. Disabling summaries can
restore the known timeouts; use isolated probes rather than enabling them in the main suite.

## Reproduction

```sh
sbt --client "dtest ctml/ctmlRecursiveSumm"
sbt --client "dtest ctml/ctmlNestedListResult"
sbt --client "dtest ctml/nofib/"
sbt --client "dtest ctml/"
sbt --client shutdown
```

The baseline contains 54 passing CTML files, including marked limitations. Diff-test output changes
must be reviewed: a passing suite includes explicit `:fixme` and `:breakme` cases and does not mean
all inference or soundness issues have been resolved.

## Validation result

`sbt --client "dtest ctml/"` passes all 58 files in 9.1 seconds. The main recursive-summary file
processes in 0.9 seconds; every file is below five seconds in this run. Existing flow, cyclic-bound,
and extrusion tests retain their expected outcomes. Two existing `:fixme` cases become successful
checks: annotated direct list length and independent polymorphic uses of a local recursive closure.
The generated output has been reviewed; no shared MLscript infrastructure is changed.
