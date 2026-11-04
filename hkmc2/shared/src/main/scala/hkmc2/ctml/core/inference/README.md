# Functions and bindings

The parser retains the elaborator's term-variable identities. Names are for display; resolved
references use identity for lookup and dependency analysis. Synthetic literal and operator
references select their prelude declarations, independently of local shadowing.

A binding block becomes dependency-ordered `BindingGroup`s. Function closures may refer forward.
Each value initializer additionally depends on the preceding value initializer, preserving their
evaluation order. Initialization dependencies are conservative: a cyclic component is accepted only
when all initializers are syntactic closures, possibly ascribed. `ELet` gives a component its local
scope, while the test entry point exports a top-level component to the environment.

For a recursive component, inference first checks all bodies with shared, uninstantiated variables
for the recursive names. It closes the type of the resulting group transformer, then applies an
intrinsic fixed-point signature. For two unary functions, this signature has the shape:

```
forall A, B, C, D.
  ((A -> B) -> (C -> D) -> [(A -> B), (C -> D)]) -> [(A -> B), (C -> D)]
```

The signature follows the actual curried parameter lists, preserving annotations and Unit domains
for empty lists. Its known function shapes avoid nested polymorphic cycles on whole-function
variables. Binary products package multiple results internally; they do not change source tuple
syntax. Recursion does not resolve or invoke a term named `fix`.

The transformer and its fixed-point application need distinct treatment of levels. The transformer
is generalized in an inner level; the application stays at the caller's level, where its instance
can be solved against the transformer's assumptions. A local recursive component is subsequently
generalized as a whole before its types are passed to the continuation. Each exported result uses
the same component clauses, preserving captured dependencies.

Ordinary local bindings use the lambda-calculus let rule: infer the continuation as an abstraction
and apply its type to the initializer's inferred type. Initializer clauses remain obligations even
when the bound name is unused. Closing an extra level around an arbitrary initializer would hide
those obligations in an unused type. Function bodies, in contrast, are delayed by closure creation.

Annotated lambda parameters still receive fresh variables, bounded by their annotations, so they
can be refined in branches. The inferred function is also checked against `Annotation -> Top`:
it must accept the entire annotated domain, rather than merely an inferred subset.

Bindings remain immutable. Destructuring bindings, local type declarations, expression statements,
rest/contextual parameters, and explicit type parameters on function definitions are unsupported.
