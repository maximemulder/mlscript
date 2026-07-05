# CTML

## Description

This project is a fork of MLScript, a research programming language. This specific fork is aimed at
the study and implementation of CTML, a research type system. This section describes CTML-specific
instructions, the section below is the original MLScript `AGENTS.md` file.

## Scope

CTML a type system that combines subtyping, set-theoretic types (unions, intersections, negations),
first-class polymorphism, and first-class constrained types. The combination of these features also
aims to exhibit flow-sensitive type inference through context disjunction.

Since CTML is a research programming language, the type checker does not necessarily need to be
production-grade. What matters the most is that it correct and easily maintainable.

## Paper

The paper related to the system implemented in this project  can be found in
`/media/data/HKUST/Research/ctml/latex` (on the main development machine). Both the paper and
implementation are still work-in-progress, however, they should eventually converse to the system
at the abstract level, although specfic differences may still exist. The paper can be used as a
reference for the implementation, but change suggestions for the paper may also be made if the
implementation is deemed more correct in some cases.

## CTML code

The CTML code is located in `hkmc2/shared/src/main/scala/hkmc2/ctml`. Most of the code outside of
this directory is irrelevant to CTML (although it does contain some common infrastructure).

## CTML tests

The CTML tests are located in `hkmc2/shared/src/test/mlscript/ctml`.
The tests outside of this directory are irrelevant by CTML.

Each test file is generally expected to run in less than 5 seconds. Some worst-case scenario might
run for more than 10 seconds, but these are problematic and should eventually be fixed. A test file
running in more than 30 seconds should be considered as a timeout, in which case the exact test
that is timing out should be located, and eventually fixed or commented.

## CTML options

Since CTML is a separate type checker, many of the options from MLScript are not available for
CTML. However, whenever you run the CTML examples, you can use the options  listed in the
`hkmc2/shared/src/main/scala/hkmc2/ctml/config` module to alter the type checker behavior or
display various debugging information.

Example:
```
:ctml

:ctml-dbg infer

val i = 1
```

# Using the MLscript code base


## Workflow

It is best to leave the SBT shell open (by just typing the `sbt` command line)
and issue commands directly in that shell, as SBT startup is very slow.

Before running individual tests, make sure to first run `ctest`,
so that the compilation-tests (in `hkmc2/shared/src/test/mlscript-compile/`) are run
and produce the JS files that are needed at runtime for the other tests (in `hkmc2/shared/src/test/mlscript/`).
Note that this only affects the main compilation tests;
there is also `cntest` for nofib compilation tests, `catest` for application compilation tests,
and `cwtest` for WASM compilation tests.

Before finishing your work, run `hkmc2AllTests/test` to make sure the codebase compiles and all tests succeed.

After you are done fixing all the problems and all the tests pass,
*you need to commit the resulting golden test output changes*.
Any commit that does not include the latest changes to test outputs will fail the CI.

Please also read the files in `.github/skills/hkmc2-difftests`.


## Design Philosophy

You are working on an industry-strength compiler codebase.
Although its test suite is extensive, tests alone are not enough to determine the quality of a change;
the goal is not just to "make everything green".

When making a change, you must reason about whether it is *completely correct*,
including with respect to edge cases that may not be covered by existing tests
or that will only become relevant later:
you should take into account the future maintainability of the code,
preferring principled designs over ad-hoc short-term solutions.

The worst possible outcome would be to introduce changes that might lead to silent miscompilation.
If you are not sure about whether some invariants you need do hold,
assert them using `softAssert` or `assert` (if `softAssert` is not available).
If you know that some invariants temporarily do not hold but will be fixed in the future, use `softTODO`.

It is very much preferable to add regression tests and `:fixme`s for bugs that should be fixed later
rather than to introduce temporary workarounds that bypass bugs just to make the tests pass.


## Coding Style

Never use `asInstanceOf` unless absolutely necessary. If you find yourself using `asInstanceOf`, it's a sign that your code may need to be refactored to be more type-safe.

Never use non-local mutation unless absolutely necessary.
In particular, you should always refrain from adding new global mutable state to things like the Symbol classes
unless there is no reasonable alternative, in which case you should document in the code
exactly where the mutable state is supposed to be defined and used.

Never use default arguments in core business logic.
Default arguments should be reserved for user-facing APIs.

Keep it DRY: if you find yourself copying and pasting code,
consider refactoring it into a reusable function or class.
The goal of minimizing code duplication is to improve maintainability:
the logic for handling cases that ought to be similar should be centralized.

**Document your code**:
Use comments to explain the intent behind complex logic,
especially if it is not immediately clear from the code itself.
When appropriate, explain the history of what led to the current implementation,
especially if it involves non-obvious decisions/trade-offs
or if alternative approaches were considered and rejected.


## Editing Style

Do not remove existing `end` markers.

Never strip indentation whitespace.

Empty lines in this project are usually significant.
They help to visually identify blocks and groups of blocks of code
(multiple empty lines separates groups of blocks hierarchically).
You should never remove pre-existing empty lines as part of your changes
unless they are at the very end of the file:
while we often leave some there for easier future editing by programmers,
it seems AI agent toolings like `apply_patch` reliably removes them anyway, which is okay
(note: this is discussed at https://github.com/openai/codex/issues/21121#issuecomment-4608911199).

**Important:** When working on a PR, make sure to check the diff of the whole PR (including all commits)
to ensure that no needless empty-line changes are included in the PR. If you find any, please remove them.


## Manipulating IR representations

When working with IR representations, please refer to the "Important design notes" in `hkmc2/shared/src/main/scala/hkmc2/codegen/Block.scala`.
