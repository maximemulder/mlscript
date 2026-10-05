# CTML

This repository is a fork of MLscript dedicated to CTML, a research type system that combines
subtyping, set-theoretic types (unions, intersections, negations), first-class polymorphism, and
first-class constrained types. Together, these features aim at flow-sensitive type inference
through context disjunction.

The CTML type checker is the subject of this fork. The rest of the code base (lexer, parser,
elaborator, diff-test harness...) is MLscript infrastructure, which is shared with the upstream
repository and regularly rebased on it.


## Goals

CTML is a research type checker: it does not need to be production-grade, but it must be correct
and easily maintainable.

The implementation evolves incrementally: prefer small, staged changes over rewrites of the
checker core.

The paper (`/media/Data/HKUST/Research/ctml/latex`) and the Lean mechanization
(`/media/Data/HKUST/Research/ctml/lean`) are on the main development machine, outside of this
repository. Use them as references for the direction of the system, not as specifications to
mirror exactly: all three are still work in progress, and specific differences may remain. When
the implementation seems more correct than the paper, suggest a change to the paper.


## Layout

- `hkmc2/shared/src/main/scala/hkmc2/ctml/`: the type checker.
  - `parser/`: the translation of elaborated MLscript terms to CTML terms.
  - `types/`: types, constraints, clauses, and typing contexts.
  - `core/`: type inference, subtyping, context joins, and type simplification.
  - `config/`: the options of the `:ctml-cfg` and `:ctml-dbg` commands.
  - `test/`: the entry point of the diff tests.
- `hkmc2/shared/src/test/mlscript/ctml/`: the CTML tests, and the CTML prelude `ctmlPrelude.mls`.
- `hkmc2DiffTests/src/test/scala/hkmc2/InvalmlDiffMaker.scala`: the CTML diff-test commands.


## Tests

The CTML tests are diff tests: `.mls` files made of blocks separated by blank lines, whose output
is written below each block as `//│` lines. The test runner rewrites these lines in place, so a
change in output is not a failure by itself: review it with `git diff` after each run.

A CTML test file starts with `:ctml`. A block may be preceded by commands:
- `:te`: the block is expected to raise a type error.
- `:fixme // FIXME: <reason>`: the block raises errors that it should not.
- `:breakme // FIXME: <reason>`, followed by `:te`: the block should raise a type error, but does
  not.
- `:ctml-cfg <options>`: change the configuration of the type checker.
- `:ctml-dbg <options>`: print debugging information. The type checker then stops after
  `maxStepCount` steps (`config/config.scala`), which truncates long traces.

The options are listed in `hkmc2/shared/src/main/scala/hkmc2/ctml/config/parser.scala`. Example:
```
:ctml

:ctml-dbg infer

val i = 1
```

Run the CTML tests with `sbt --client "dtest ctml/"`. To run some files only, filter them by a
part of their path without the `.mls` extension: e.g. `sbt --client "dtest ctml/ctmlFlow"` runs
both `ctmlFlow.mls` and `ctmlFlowWeirdMatch.mls`. The first call starts an sbt server in the
background, which later calls reuse: stop it with `sbt --client shutdown` when you are done.

Changes confined to `ctml/` only need the CTML tests: do not run `ctest` or the rest of the test
suite. When changing shared code, including the diff-test harness, also run
`sbt --client ctest`, and then `sbt --client hkmc2DiffTests/test`.

Each test file should run in less than 5 seconds. Some worst cases take more than 10 seconds,
which should eventually be fixed. A file that takes more than 30 seconds has timed out: locate the
block that times out, and fix it or comment it out with a `FIXME` comment explaining why.

The CTML tests may contain known failures or time-outs while the type checker is under active
development. Before a refactor, run the CTML tests to record a baseline of the failing files, and
compare later runs against it rather than assuming that the suite is green. Golden output changes
in CTML files may be committed as an intentional baseline before a larger refactor, but never
commit the unrelated golden output changes of other tests, such as the WASM tests.


## Code conventions

- Soundness comes first: never make the type checker accept a program just to make a test pass.
  When a test exposes a bug that you do not fix, mark it with `:fixme` or `:breakme` rather than
  working around it.
- Prefer principled designs over special cases, and factor out shared logic rather than
  duplicating it.
- Assert the invariants that you rely on but that the types do not guarantee.
- Do not use `asInstanceOf`.
- Do not add global mutable state beyond the existing configuration (`config/`) and fresh type
  variable counter (`core/var_/`).
- Do not keep code for compatibility (deprecated aliases, unused parameters, forwarding
  definitions...): this code has no external users, so update every use instead.
- Every definition has a `/** ... */` doc comment. Wrap comments at 100 columns.
- Do not use `end` markers, and do not indent blank lines.


## Comments

Comments describe the code as it is, and why it is so, never how it came to be. Do not write
comments such as "this used to...", "previously...", "now...", "X was tried...", or "fixed X":
the history of the code belongs in commit messages.

The reasons behind a design are still worth writing down, in the present tense: invariants,
counterexamples that a simpler approach gets wrong, and deliberate differences from the paper.
For example, instead of:
```
// This rule used to come after the rules of rigid variables, so that `α ≤ {α ≤ Int} ⟹ Int`
// was explored as `⊤ ≤ {α ≤ Int} ⟹ Int`, which fails.
```
write:
```
// This rule comes before the rules of rigid variables, which depend on the assumed bounds:
// `α ≤ {α ≤ Int} ⟹ Int` would otherwise be explored as `⊤ ≤ {α ≤ Int} ⟹ Int`, which fails.
```

The same applies to the comments of tests: describe what a test checks, not what the type checker
used to do.


## Shared code

Keep the changes outside of `ctml/` minimal, since each of them may conflict when rebasing on
upstream MLscript. In these files, follow the upstream conventions, which differ from those of
CTML: keep `end` markers and the indentation of blank lines, and do not reformat the code that you
do not change. The upstream instructions for these files can be read with
`git show upstream/hkmc2:AGENTS.md`.
