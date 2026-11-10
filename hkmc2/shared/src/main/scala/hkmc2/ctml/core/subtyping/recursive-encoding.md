# Constrained recursive types: implementation and proof report

Date: 2026-11-10.

## Outcome and scope

CTML can check the finite fold/unfold derivations for the constrained encoding of lists without
adding a recursive type constructor or a coinductive subtyping rule. The implementation adds:

1. Capture-avoiding substitution of arbitrary types for quantified variables.
2. Explicit term-level type arguments, including impredicative and partial instantiation.
3. A target-directed instance for universals whose body, after leading guards, is their bound
   variable. The instance is checked by proving every substituted guard.

The regression examples check the concrete list equation, equivalence with the equality-bounded
encoding, element covariance, mixed-element lists, checked fold/unfold functions, a recursive
generator, `length`, and `map`. These are type-checking tests, not executions of the programs.

There are two material limitations. Compact recursive functions still need inference improvements;
the working examples supply the fixed-point type and local annotations. Also, an adversarial
negative encoding exposes an existing cyclic-constraint problem, recorded as a `:breakme` below.
The report is a derivation and mechanization plan, not a machine-checked soundness proof of the
implementation. The paper and Lean repository have not been edited.

## Declarative construction

Write `F(T)` for a type expression with one distinguished argument, not for a new object-language
type operator. Assume it is well formed, commutes with the required renamings/substitutions, and
is monotone in every context used by the derivation:

```text
Γ ⊢ S ≤ T  implies  Γ ⊢ F(S) ≤ F(T).
```

Define, with the bound variable chosen fresh:

```text
R_F = ∀X. {F(X) ≤ X} ⇒ X.
```

For lists, fix an element type `A` and take:

```text
F_A(X) = Nil ∨ (Cons ∧ {head: A} ∧ {tail: X}).
```

Monotonicity follows from covariance of immutable fields, unions, and intersections. No
inheritance relationship between `List` and its constructors is assumed. `Nil` and `Cons` are
ordinary disjoint tags, and payload requirements remain structural.

### Elimination into a closed carrier

If `Γ ⊢ F(Y) ≤ Y`, universal elimination at `Y` gives:

```text
Γ ⊢ R_F ≤ ({F(Y) ≤ Y} ⇒ Y) ≤ Y.
```

The second step is constrained-left elimination using the supplied proof of the guard. This
establishes that `R_F` is below every definable `F`-closed carrier, without unfolding any recursive
definition.

### Fold

Choose a fresh `X` and assume `F(X) ≤ X`. The preceding elimination gives `R_F ≤ X`. Monotonicity
and transitivity then give:

```text
Γ, X, F(X) ≤ X ⊢ F(R_F) ≤ F(X) ≤ X.
```

Discharge the guard and generalize `X` to obtain `Γ ⊢ F(R_F) ≤ R_F`. The freshness condition is
`X ∉ FV(Γ, F(R_F))`; the binder inside `R_F` is alpha-renamed independently.

The generalized introduction used here is derivable from the paper's primitive rules. Starting
with `Γ, X, c ⊢ S ≤ T`, use constrained covariance and constrained-right introduction to derive
`Γ, X ⊢ S ≤ {c} ⇒ T`. Then use universal covariance, universal-right introduction, and
transitivity, with `X` fresh for `Γ,S`, to derive `Γ ⊢ S ≤ ∀X. {c} ⇒ T`.

### Unfold

Apply monotonicity to the already established fold judgment:

```text
Γ ⊢ F(F(R_F)) ≤ F(R_F).
```

Thus `F(R_F)` is a closed carrier. Elimination into that carrier gives:

```text
Γ ⊢ R_F ≤ F(R_F).
```

The witness is the impredicative type `F(R_F)`. The proof is finite: it uses the previously proved
fold judgment and does not assume the unfold judgment being proved. Together these results show
that `R_F` is a least fixed point with respect to derivable subtyping among the admissible
carriers. They do not establish termination of terms or a dependent induction principle.

### Equality-bounded variant

For the original proposal, define:

```text
E_F = ∀X. {F(X) ≤ X, X ≤ F(X)} ⇒ X.
```

Under its two guards, the first guard suffices to derive `R_F ≤ X`; introduction gives
`R_F ≤ E_F`. Conversely, instantiate `E_F` at `R_F`. Both guards are the established fold and
unfold judgments, so `E_F ≤ R_F`. Hence `E_F ≡ R_F` for monotone `F`.

The one-sided construction is preferable as the primary definition because its elimination
principle requires only a closed carrier. The equality-bounded variant needs the fixed-point
lemmas to exhibit the useful instance.

### Precedent

Jenkins and Stump's *Monotone Recursive Types and Recursive Data Representations in Cedille*,
Section 3.4 and Figure 15, uses `∀X. Cast (F X) X ⇒ X`, where the inclusion evidence is erased.
Its roll/unroll operators erase to identity functions. This is a close precedent for replacing
erased inclusion evidence by CTML constraints; it does not transfer Cedille's metatheory to
CTML. The derivations above are the proposed adaptation to CTML's rules.

Reference: <https://arxiv.org/abs/2001.02828>.

## Implementation rules

### Explicit type application

`ETypeApp` represents expressions such as `id[Int](1)`, `pair[Int][Str]`, and
`fix[IntList -> Int](...)`. The parser translates the existing MLscript type-application syntax;
no shared parser or elaborator changes are needed.

`core/inference/instantiation.scala` consumes supplied arguments against leading universal
binders, exposing transparent rigid aliases and retaining intervening guards. Inference then
proves the remaining leading guards in source order and returns their body. For one binder,
the derived rule has the shape:

```text
Γ ⊢ e : ∀X. {c₁, ..., cₙ} ⇒ T
Γ ⊢ cᵢ[S/X]   for each i
-------------------------------------
Γ ⊢ e[S] : T[S/X]
```

This combines ordinary universal elimination, constrained-left elimination, and subsumption.
Generated guard obligations are retained in the returned inference clauses, together with all
obligations from the operand expression. An invalid bound cannot be assumed or discarded merely
because the result is unused. Guards beneath an uninstantiated universal remain beneath it.

Explicit instantiation requires a visible universal, possibly under transparent aliases or
guards. It does not guess a universal from an arbitrary inference variable or subtype bound.
Too many type arguments and arguments applied to a monomorphic type are rejected. Operationally,
type application is erased. Free-variable analysis, recursive-initializer classification, and
safe UCS substitution treat it accordingly.

### Target-directed subtyping

For the general syntactic shape `∀X. {c₁, ..., cₙ} ⇒ X`, the checker tries the target `T` itself
as a witness:

```text
Γ ⊢ cᵢ[T/X]   for each i
-----------------------------------------
Γ ⊢ (∀X. {c₁, ..., cₙ} ⇒ X) ≤ T
```

There is no test for a list name, recursive equation, or positive occurrence here. This is a
sound derived universal-elimination rule for arbitrary guards, provided their checking is sound.
Monotonicity is required by the mathematical fixed-point theorem, not by this instantiation rule.

The displayed rule is declarative. In the algorithm, a guard can generate output clauses rather
than be discharged in the input context alone. Guard `i` is constrained in the context extended
by the preceding outputs, and the candidate returns their concatenation. Its soundness claim is
therefore conditional on those output obligations, with the existing treatment of fresh-variable
scopes. This distinction also applies to explicit type application; the new code does not treat
successful constraint generation as a separate proof that every output system is satisfiable.

The attempt occurs before replacing a rigid target by its bound or choosing a union branch. It
also tries a whole universal target before right-universal introduction, which is necessary to
use the one-sided encoding itself as the witness in the equality-encoding comparison. Failure
falls back to the existing rules: the target is a useful candidate, not the only legal instance.

The helper discharges the substituted guards directly. Sending the guarded identity instance
back through ordinary subtyping would allow target decomposition to hide the chosen witness
again. The result body is already the target by construction, so reflexivity supplies the final
step.

`SubtypingTrail.instances` records attempts on the current search path. Repeating an attempt
skips that candidate; it never proves its guard or the enclosing judgment. The existing
variable-judgment trail remains separate. Contexts are immutable, so bounds from a failed
candidate are not retained by the fallback. This is a local repetition guard, not a proof that
all impredicative subtype search terminates.

### Substitution

`core/type_/impls/substitute.scala` accepts an arbitrary replacement type. It stops beneath a
binder shadowing the substituted variable, and renames an intervening binder if it would capture
a free variable of the replacement. Both operands of every constraint are traversed. Existing
fresh-variable instantiation uses the same operation with `TVar` replacements.

For example, substituting a free `Y` for `X` in `∀Y. X -> Y -> X` must produce
`∀Z. Y -> Z -> Y`, not `∀Y. Y -> Y -> Y`. This matters for both explicit arguments and
target-directed witnesses.

## Program examples and remaining inference work

The tests use ordinary `LNil` and `LCons` constructors and the encoded `IntList`, with
`IntLayer = F_Int(IntList)`. `roll` and `unroll` are checked identity functions, not declared
coercions. Explicit unfolding is expressed as `(xs as IntList)[IntLayer]`.

The recursive generator checks with a supplied fixed-point type. Consumers additionally need
their recursive-function invariant established outside the pattern branches, with ascriptions
at uses that would otherwise generate more general branch-dependent constraints. For example:

```mlscript
val length = fix[IntList -> Int]((self: IntList -> Int) => xs =>
  if (xs as IntLayer) is
    LNil() then 0
    LCons(_, tail) then 1 + (self as (IntList -> Int))(tail))
```

`mapInt` is checked and composed with the generator and `length`. Another `map` body is checked
for arbitrary rigid element types `A` and `B`, with type `(A -> B) -> ListA -> ListB`. This is
a schema-level generic check: those rigid types are declared in the test environment. It is not
yet a single exported polymorphic function using parameterized `List[A]` aliases. Such aliases
and explicit type parameters on function implementations remain separate frontend work.

The compact `fun inferredLength(xs: IntList): Int = ...` form is retained as a `:fixme`.
A recursive generator variant with an inferred signature and a recursive-result ascription
exceeded 30 seconds; its small reproducer is commented out with a FIXME. Neither failure is
used to justify accepting a program. Improving annotation propagation and inference through
constructor branches is the next step toward readable Nofib ports; the GCD driver is unchanged.

## Existing negative-encoding problem

The adversarial type and judgment are:

```text
N = ∀X. {¬X ≤ X} ⇒ X
N ≤ ¬N
```

The checker reports success. The same result occurs with target-directed instantiation disabled.
Debug output exposes residual flexible bounds `α ≥ ¬α` and `α ≤ ¬N`; a value-level ascription
of the identity function to `N -> ¬N` is also accepted. The issue is therefore not evidence of
a valid fixed-point law for negative occurrences, and raw `OK` output alone is insufficient to
establish that generated constraints have a solution.

Under the intended Boolean/set interpretation, `¬X ≤ X` forces `X = Top`, so `N` denotes `Top`
and the displayed judgment would require `Top ≤ Bot`. The regression is marked `:breakme` in
`ctmlTargetInstantiation.mls`. It is an existing consistency/discharge limitation involving
cyclic flexible constraints; the exact solver repair is outside this change. It must remain
visible when assessing implementation soundness.

The concrete positive-list fold and unfold checks were separately inspected with clause output
enabled: their outputs contained only fresh rigid declarations, with no deferred flexible
constraints. The finite declarative derivations above remain the basis for the proposed Lean
theorems, independently of the implementation tests.

## Suggested Lean proof decomposition

1. Prove capture-avoiding substitution and weakening compatibility for the chosen type scheme.
   Represent `F` metatheoretically, or start with the concrete list body. The Scala `TApp`
   covariance examples do not require introducing higher-kinded syntax into Lean.
2. Derive guarded universal introduction from `Subtype.forallRight`, `forallCovariant`,
   `constrainedRight`, `constrainedCovariant`, and `trans`, with explicit freshness conditions.
3. Prove closed-carrier elimination using `forallLeft`, `constrainedLeft`, and `trans`.
4. Prove list-body monotonicity using the record and lattice rules. For a generic theorem,
   require context-stable monotonicity and the substitution/weakening laws for `F` as premises.
5. Derive fold, then the lifted fold, then unfold. Establish leastness and equivalence of the
   equality-bounded variant as corollaries. No coinductive hypothesis is required.
6. Prove the target-instance rule as a finite derived rule. Its algorithmic soundness obligation
   must account for the clauses generated by each guard and their sequential composition.
7. For explicit type application, either add an erased term form or describe it as checked
   elaboration to an existing ascription. Preserve operand obligations and prove each exposed
   guard; do not turn a guard into an assumption merely because an argument was supplied.
8. Keep the cyclic-flexible-constraint counterexample separate from the positive recursive-type
   theorem. The declarative derivation does not validate that implementation behavior.

## Validation

The baseline contained 44 passing CTML files. The final suite contains 47 files, with the known
inference limitation marked `:fixme` and the existing negative-encoding acceptance marked
`:breakme`. The tests cover:

- Explicit, partial, and impredicative type arguments; bad arguments and arity; guard discharge
  even for unused results; capture avoidance and shadowing.
- Target witness success, fallback to another instance, scoped assumptions, multiple guards,
  and rejection of circular rigid-bound proofs.
- Abstract and concrete fold/unfold, equality-encoding equivalence, element covariance, and the
  distinction between mixed-element lists and unions of homogeneous list types.
- Checked list helpers, recursive producers and consumers, arbitrary-element mapping, and
  rejection of malformed tails and incompatible map functions.

Only CTML implementation and test files are changed. No shared harness changes or paper/Lean
changes are included.
