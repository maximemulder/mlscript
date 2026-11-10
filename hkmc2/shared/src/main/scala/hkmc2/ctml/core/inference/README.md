# Functions and bindings

The parser retains the elaborator's term-variable identities. Names are for display; resolved
references use identity for lookup. Synthetic literal, operator, and fixed-point references select
their prelude declarations, independently of local shadowing.

Immutable bindings are checked in source order. A definition is available to subsequent definitions
and expressions. Forward references and mutual recursion are unsupported: references to definitions
that have not been checked fail variable lookup, including inside closures.

Direct recursion is translated to `fix (self => body)`, using the prelude signature
`forall A. (A -> A) -> A`. Only syntactic closures, possibly ascribed, may refer to themselves;
recursive value initializers require an initialization semantics and are rejected.

Local bindings are translated to lambda applications. Initializer clauses remain obligations even
when the bound name is unused. Closing an extra level around an arbitrary initializer would hide
those obligations in an unused type. Function bodies, in contrast, are delayed by closure creation.
There is no separate binding or recursion rule in inference. A local fixed-point application is
not generalized independently of its continuation, so uses at different types may merge their
results. Top-level definitions are generalized before they enter the environment.

Annotated lambda parameters receive fresh variables, bounded by their annotations, so they can be
refined in branches. The inferred function is also checked against `Annotation -> Top`: it must
accept the entire annotated domain, rather than merely an inferred subset.

Bindings remain immutable. Destructuring bindings, local type declarations, expression statements,
rest/contextual parameters, and explicit type parameters on function definitions are unsupported.
