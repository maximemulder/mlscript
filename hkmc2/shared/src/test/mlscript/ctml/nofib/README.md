# Porting Nofib to CTML

## Purpose and priorities

The goal is to test CTML **type inference** on realistic programs. Port Nofib file by file,
preserving its algorithms and leaving ordinary function definitions unannotated. A faithful port
that exposes an inference failure is a useful test; successful inference is not a prerequisite
for continuing the port.

Read the repository's `AGENTS.md` for checker conventions and test instructions. This document
records the port-specific decisions and the handoff state. Update it when a port advances or a
design decision changes.

## Handling gaps

Distinguish these cases before changing the checker or rewriting a benchmark:

| Gap | Action |
| --- | --- |
| The language cannot express a source construct | Report the gap and propose a feature or a source rewrite to the user before implementing that decision. |
| The translated program is expressible but inference or subtyping fails | Preserve the unannotated program, record the failure, and continue porting. Defer solver work unless explicitly requested. |
| A standard-library type or operation is missing | Put shared support in `NofibPrelude.mls`; infer ordinary helper implementations and declare genuinely primitive operations with precise types. Discuss new representation choices with the user. |

Keep benchmark functions unannotated. For widely used library helpers whose inference failure
or timeout blocks downstream porting, a precise manual signature is an allowed fallback:

- Preserve the faithful unannotated implementation as an inference reproducer. Use
  `:ctml-parse-only // FIXME: <reason>` for timeouts and costly inference. Put shared reproducers
  in [preludeDeferred.mls](preludeDeferred.mls), which imports the checked prelude.
- Prefer checking the implementation against the supplied signature when feasible. Keep any
  annotated or explicitly instantiated diagnostic control separate from the unannotated port.
- When implementation checking is blocked or deferred, provide a documented trusted declaration in
  `NofibPrelude.mls`. State that its implementation is not checked by that declaration, and link
  relevant diagnostic controls. Do not replace its contract with an unchecked or dynamic type.
- Test callers against the supplied contract, including invalid arguments and result-type checks.
  Leave ordinary callers implicitly instantiated; retain independent inference failures as
  `:fixme` cases rather than weakening the signature to accept them.
- Report coverage as checking against manually supplied library signatures, not whole-program
  inference. Record each fallback in the status table below.

Result assertions and negative tests may check inferred definitions without replacing those
definitions with annotated ones. Solver work remains deferred unless explicitly requested.

Preserve source behavior, including range endpoints, empty cases, partial functions, evaluation
structure, and benchmark inputs. Document necessary representation changes. Do not replace a
list algorithm with arithmetic or another algorithm to avoid an inference limitation. CTML diff
tests check typing; a passing type test does not establish runtime equivalence.

The agent may choose the order of files and carry out routine translation within agreed decisions.
Report new feature-versus-rewrite decisions and substantial checker design changes to the user
before implementing them. An inference failure alone does not require such a design change.

## Agreed choices

- Add shared support as needed. Several benchmarks can be ported together when they fit the
  agreed conventions.
- Keep direct recursion. Mutual recursion and richer binding support are deliberately out of
  scope for now; revisit them only when a concrete source program warrants a decision.
- Represent triples as nested pairs, `[a, [b, c]]`, and destructure them with nested tuple
  patterns. CTML supports binary tuple patterns without spreads in `if ... is ... then` matches;
  direct parameter and `let` patterns remain outside this port's scope. Precisely typed trusted
  `fst` and `snd` primitives are also available in the prelude.
- Tuple-pattern fallthrough can expose missing product-disjointness subtyping. The focused
  `ctmlTuplePatterns.mls` tests retain these cases as `:fixme`; unconditional destructuring in
  the GCD kernel does not require these judgments.
- Represent source laziness with trusted polymorphic `delay` and `force` in the Nofib prelude.
  A handle has a nominal `Delayed` tag and carries its result type through the immutable
  `resume` function. Forcing an uncached handle evaluates its initializer and stores the result
  on success. Subsequent forces return the cached result. CTML tests check the handle and result
  types.
- Use readable constructor patterns for lists. An abstract list API with trusted `caseList`
  callbacks is not the chosen porting approach. CTML supports nonrecursive constructor
  declarations and constructor patterns; the list tail's recursive relationship is a type-level
  question rather than a recursive constructor declaration.
- Explore recursive lists through the existing constrained polymorphic encoding. The encoding
  and explicit-instantiation support are implemented; native recursive types are not a
  prerequisite for continuing the port. Generic source-level alias syntax or other missing
  conveniences still need a decision if a port requires them.
- Automatic recursive-witness selection and consumer inference precision remain open. The
  authorized consumer-performance investigation localizes the sampled timeouts to closing
  recursion over guarded bounds. Follow up on that bottleneck when expanding inference coverage.

## File-by-file workflow

1. Read the original benchmark and the helpers it uses. Identify its input, expected behavior,
   and dependencies before translating. Copy only the selected benchmark and needed helpers.
2. Translate into this directory using existing CTML constructs and approved rewrites. Record
   the original source path and representation choices in the file. Share reusable definitions
   through `NofibPrelude.mls`; CTML's `:import` already works.
3. Keep benchmark functions and ordinary helpers unannotated, except for the documented library
   fallback above. Classify each obstacle using the
   table above. For an inference failure, retain a small reproducer when useful, but do not let
   an open-ended solver investigation block the port.
4. Mark valid programs rejected by the checker with `:fixme // FIXME: <reason>`. Mark invalid
   programs accepted by it with `:breakme // FIXME: <reason>` followed by `:te`. These commands
   apply to diff-test blocks, so separate successful definitions from failing uses where useful.
5. Try inference on the translated definitions. For a timeout, isolate the offending block and
   add `:ctml-parse-only // FIXME: <reason>`. The flag runs MLScript elaboration and CTML
   translation for one diff-test block. Skipped definitions supply no typing bindings. If a
   dependency lacks a type, use a documented library signature where the fallback policy applies
   or keep its caller under the same flag when its names resolve. Keep terms with missing
   dependencies in comments with an explanation. The flag applies to ordinary test blocks and
   leaves imported preludes type checked.
6. Run focused tests, review the generated `//│` output, then run the CTML suite for executable
   changes. Follow `AGENTS.md` for baseline comparisons and any shared-code changes. Aim for
   under five seconds per file and skip inference for known timeout cases.
7. Update the status below with translated sections, active failures, disabled blocks, and the
   next concrete task. Distinguish translation completeness from successful inference.

Useful commands from the repository root:

```sh
sbt --client "dtest ctml/nofib/"
sbt --client "dtest ctml/ctmlRecursiveProducer"
sbt --client "dtest ctml/"
sbt --client shutdown
```

The runner rewrites golden output in place. Review `git diff` after a run, including expected
failures; a successful suite can contain explicitly marked inference limitations. Stop the sbt
server when finished. Do not include unrelated golden-output changes.

## Current status and next task

| File or area | State |
| --- | --- |
| [gcd.mls](gcd.mls) | Source translation complete, inference coverage partial: `g` and `gcdE` infer successfully. `max_` inference alone exceeds the 25-second limit; the extracted nested comprehension also times out. Both reproducers and the full driver check through translation with `:ctml-parse-only`. Independent checks of both tuple-mapping stages compose successfully through the trusted `map` contract, with an integer-list result assertion and an invalid-input test. |
| [cryptarithm1.mls](cryptarithm1.mls) | Source translation complete, inference coverage partial: `expand` and the exact ten-element `condition` pattern infer successfully. `addj` exceeds the 25-second test limit. It and its dependent permutation generation and driver check through translation with `:ctml-parse-only`. The source input remains `1`. The MLscript runtime reference reports a stack overflow, not a successful benchmark result. |
| [cse.mls](cse.mls) | All source definitions are present. The state combinators, `incr`, `newlyDefined`, tree constructors, and examples 0–3 infer without annotations. Example 4 checks with the trusted `scanl` contract. Six shared or local source list helpers and all four stateful traversals exhaust a 20,000-call diagnostic. The local list helpers, stateful traversals, tree labelling, `findCommon`, and example 5 check through translation. Graph construction and the visited-state check infer against the trusted list contracts. The merge pipeline and the driver with input `6` check through translation. |
| [scc.mls](scc.mls) | The complete SCC algorithm and input `0` are translated. The fixed graph and edge reversal infer unannotated, and transposing that graph checks against the trusted `map` contract. Neighbor lookup and DFS exhaust a 20,000-call diagnostic, even with temporary `inList` and `append` signatures for DFS. The trusted `inList` contract supplies membership. Neighbor lookup, both traversals, and the input-`0` driver check through translation. |
| [lcss.mls](lcss.mls) | The source algorithm and fixed progressions `(1, 2, 60)` and `(30, 31, 90)` are translated. The individual score update infers unannotated. Row traversal, initialization, split index selection, and recursive reconstruction exhaust the 20,000-call diagnostic. Reconstruction hits the limit even with temporary contracts for its dependencies. The row traversal, initialization, and split index selection check through translation. Reconstruction and the complete driver also check through translation. The source runtime reference yields the integers from 30 through 60. |
| [integer.mls](integer.mls) | The progression and nine arithmetic and comparison operations infer unannotated. A result assertion after matching the inferred progression retains an active `:fixme`. The nested traversal exhausts the 20,000-call diagnostic. The nested traversal, the two passes, and their driver check through translation with the adapted input `700000001`. |
| [secretary.mls](secretary.mls) | The infinite random-stream producer infers unannotated. A check forces two successive cells and recovers both integer scores. Lazy filtering and taking, deduplication, the strategy, and the trial and result traversals exceed bounded diagnostics. The per-seed simulation checks through translation. Deduplication and the lazy traversals are in `preludeDeferred.mls`. The strategy and result traversal remain commented because those helpers are unavailable in this file. The driver retains input `50`; source runtime output is `"[0.3,0.3,0.3,0.34,0.36]"`. |
| [life.mls](life.mls) | The cell rule, lazy repetition, and cell rendering infer unannotated. The forced repetition's outer shape checks, while a result assertion after matching it retains an active `:fixme`. A concrete call to `glue` checks its outer result shape using trusted `append`, and passing that result to `listLen` infers an integer. The seed row times out and core traversals exhaust bounded diagnostics. The seed, initial board, and row shifts check through translation. The generation and display pipelines remain in comments with input `15` and source runtime result `468`; they depend on helpers unavailable in this file. The empty-list case in `init` uses `raiseError("[]")`, matching the source rendering of `Nil`. |
| [calendar.mls](calendar.mls) | The Gregorian date arithmetic, fixed month tables, and blank-picture producers infer unannotated. Intermediate bindings keep month-table checking within the test budget. `firstDays` checks against trusted `scanl`, `map`, and `take` contracts. The picture dimensions `height` and `width` check on concrete pictures using trusted `listLen`. All three justification functions infer with the inferred `space` list passed to `append`. Later formatting stages and the input-`0` driver for 1993 are retained as comments. |
| [atom.mls](atom.mls) | The complete benchmark translates, including the delayed simulation, tab and newline formatting, and input `20`. Both fields of `State` use the recursive numeric vector type, with constructor checks on valid and invalid vectors. Vector addition, multiplication, scaling, rendering, and lazy zip exceed the solver budget. Those definitions and their callers use `:ctml-parse-only`. The source runtime reference ends with `"0.9322184254859536\t\n"`. |
| [mandel.mls](mandel.mls) | The complete 25-by-25 raster benchmark translates with limit 75 and the nested-pair RGB representation. Complex arithmetic, radius comparison, and construction of a concrete pixmap check. The inferred orbit producer rejects a concrete complex seed. Escape counting exceeds the solver budget, and its consumers through `main()` check through translation. |
| [mandel2.mls](mandel2.mls) | Coordinate conversion, the numeric escape loop, perimeter checks, and recursive tree construction infer. A literal nested tree checks against the constrained `MandTree` invariant. A call to the builder checks both its outer tags and the full recursive invariant. The recursive leaf traversal exceeds the solver budget, and the driver with coordinate bounds from `[0, 0]` to `[200, 100]` checks through translation. The source runtime returns `true`. |
| [banner.mls](banner.mls) | All 225 glyph rows retain their source strings and order. Character classification, glyph construction, and the punctuation, space, and slash tables check. The alphabet and digit tables exceed the solver budget. The shared trusted `atIndex` contract checks on concrete lists, and its unannotated reproducer in `preludeTests.mls` still rejects a singleton. Composing append with the nonempty right fold infers the unannotated `join`. Glyph selection and the rendering path through the one-copy driver check through translation. |
| [cryptarithm2.mls](cryptarithm2.mls) | The complete puzzle and input `1` translate. The `StateT` run field carries the inferred state and result, and concrete checks cover return, put, get, and the digit environment. The final-state extractor rejects nonempty alternatives. Deletion uses the shared contract, and the inferred list difference accepts nonempty inputs. The list traversals and state search hit solver limits or depend on unavailable inferred types and check through translation. The driver retains the source list rendering call, which has no CTML list method to check. |
| [primetest.mls](primetest.mls) | Modular exponentiation, factoring the exponent, and the combined random generator infer. The generator checks against the recursive integer-stream invariant. Tests force two successive integers and check the strict prefix and delayed suffix from the inferred splitter. Counting the digits of an inferred `chop` result and taking an integer prefix of an inferred `iterate` stream check. The modular witness test still fails at its nonempty prefix match. Parsing and the 100-trial classification pipeline check through translation with the five original candidates and input `0`. The seed guard preserves the source match error. |
| [power.mls](power.mls) | The series use `Num` for JavaScript number coefficients, including integral constants, and retain the source delay boundaries. Finite producers and the infinite `list()` check against the recursive `Series` invariant. A separate check forces two coefficients. Extraction and several recursive arithmetic operations exhaust bounded search. The remaining operations and the input-`14` driver check through translation. The driver evaluates all four source extractions in order. |
| [fish.mls](fish.mls) | Each segment uses a pair of endpoint pairs. All 120 segments retain their tile and within-tile order. Vector arithmetic, rotation, and empty-picture composition check. The inferred cycle accepts a picture containing one segment. The character-list formatter infers using `append`. The `p_tile`, `r_tile`, and `s_tile` tables infer. The `q_tile` table, recursive grid, and full drawing through input `1` check through translation. The unused formatter preserves the source string tail. The source runtime reference reports a stack overflow. |
| [cichelli.mls](cichelli.mls) | The complete 22-keyword search translates with input `6`. The key payload is a proper list of string code units, as supplied by the source. Key construction, endpoint extraction, optional bounds, partition definition, and the outer shape of the delayed range check. The inferred subset predicate accepts two nonempty lists. The keyword table and the association, histogram, and search traversals check through translation. |
| [puzzle.mls](puzzle.mls) | The complete crossing search and rendering translate with the source argument list `[2]`. Concrete checks cover the bank fields, opposite bank, member order, and history time, and reject a member in a bank field. Comparisons across tags and the minimum-history traversal exhaust bounded search. The item formatter exhausts a 20,000-call budget across its nested cases. The source runtime reference shows two optimal solutions of 17 minutes. |
| [minimax.mls](minimax.mls) | The complete game translates with the source input `"180000"`, eight winning masks, and search depth five. Piece scoring, row insertion, and construction and evaluation of a scored leaf check. Tree generation, mapping, pruning, and minimax selection infer using the trusted list contracts. Pruning an empty-child node retains a `:fixme` for the widened child list. The piece formatter infers, as does the score formatter using `append` on character lists. Move generation, board scoring, and the game loop check through translation. |
| [para.mls](para.mls) | The complete formatter translates with all ten source text fragments, including the trailing blank lines. The deque uses two lists and the layout triples use nested pairs. Concrete checks cover layout initialization, triple projections, and deque insertion. The nonempty right fold, word and line splitting, deque rebalancing, and input assembly exhaust bounded search. The exhaustive and incremental layout algorithms translate, as does the unused `fmtWith` helper that uses its argument as both text and a function. |
| [knights.mls](knights.mls) | The complete search and renderer translate with input `["8", "1"]` and the original direction and forcing order. Board creation and extension, concrete trail indexing, and two cells of the repeated lazy queue check. A malformed trail and a noninteger coordinate are rejected. The multiway move dispatch, trail traversals, and strict and lazy quicksort exhaust bounded search. The board renderer retains a `:fixme` at recursive closure. `depthSearch` matches the forced queue directly and uses a context join. Its queue match exhausts the 20,000-call solver budget, so the search and its callers check through translation. The source runtime reference shows one closed tour with zero backtracking moves. |
| [circsim.mls](circsim.mls) | The complete circuit simulator translates with eight register bits and forty cycles. Circuits, ports, and packets extend the right-nested pair encoding. Signal gates, both packet routing directions, concrete input and output port types, and singleton tree construction check. Malformed routing fields are rejected. The typed state constructor and recursive tree sweeps exceed bounded search. The two-port map composition infers. Nonsingleton tree construction retains an active inference reproducer. The critical depth calculation needs the nonempty result shape that the shared map contract does not expose. The malformed-circuit branch uses `raiseError(jsErrorMessage(circuit))`, with a trusted JavaScript Error message conversion. The source runtime reference starts with eight low signals, then has eight high signals in each remaining cycle. |
| [eliza.mls](eliza.mls) | The complete response tables, lazy reply search, and original twenty-conversation driver translate. Case conversion through the trusted string primitive, a concrete uppercase map call, the cursor projections, and forced empty and nonempty nodes check. The strict-to-delayed append forces its continuation at the end of the prefix. Recursive traversals and the full response and substitution tables have translation coverage. Trimming infers and accepts the empty list. A singleton input retains a `:fixme`. The source repeated-input branch expects a strict list even though the initial cursor is delayed. The port preserves the pattern, includes a rejection check, and the source JavaScript reproduces the match error on consecutive identical inputs. |
| [constraints.mls](constraints.mls) | All five solver variants and the original six-queens driver translate. The quicksort group uses one recursive dispatcher with four tags. Its comparator and accumulator order match the source. The CSP relation accepts two assignments with integer fields and returns a Boolean. Known conflicts carry proper integer lists, with the empty list marking a solution. The relation, assignment projections, and outcome checks infer. Relabelling checks on a leaf and on a node with a child. Union checks with both empty and nonempty first lists. Pruning and collection of leaves from a node with children retain active inference reproducers. The conflict-set collector `collect` infers an empty-list input. The search, quicksort, and table traversals have translation coverage. The source runtime reference yields `"[4,4,4,4,4]"`. |
| [pretty.mls](pretty.mls) | The full formatter translates with the source width of 80, 50 repeated words, and the trailing newline. One recursive dispatcher flattens fragments and resumes the pending stack. The representation carries the fragment through structural fields and retains integer widths and Boolean flags. Character and empty printers and concrete uses of the three binary layout combinators check. The integer and comma-space printers infer with the recursive string-list input. Indentation checks as a function from an integer and a proper string list to a proper string list. The flattener, printer folds, and driver check through translation. |
| [clausify.mls](clausify.mls) | The complete parser, normalization, clause elimination, and ten-copy driver translate. Stack token matching uses strict equality guards. A clause key combines the two bracketed string-list renderings with a comma, matching JavaScript pair coercion. Code-unit comparisons, concrete clause keys, and operator precedence check. Invalid operators use `raiseError(c)`; a malformed reduction stack raises the source match error. The recursive parser and formula traversals have translation coverage. The source runtime reference yields ten lines of `a <=`. |
| [awards.mls](awards.mls) | The complete assignment pipeline and 100-contest driver translate, preserving candidate order, score positions, and the four quicksort cases. The reverse partition finishes both groups in forward sort mode. Tuple comparison checks on concrete scores. The sorted-list comparison wrapper and list difference accept nonempty inputs. The quicksort, subsequence search, and contest driver have translation coverage. |
| [lambda.mls](lambda.mls) | Both evaluators and the original input `80` translate. State threading, temporary environments, term construction, and parenthesization check. Lookup and comparison exceed the 20,000-call diagnostic. The evaluator and printer dispatchers have translation coverage. The port preserves the comparison operands selected by source shadowing and the match error on a missing binding. Rendering the direct evaluator result infers using `append` on character lists. The source runtime prints the bracketed character lists for `Con 3240` and `3240  []`, separated by a comma. |
| [ansi.mls](ansi.mls) | The complete display program translates with one repetition and input `"testtesttest"`. Character prepending, the bell continuation, and the fractional reply column check. Cursor formatting, positioning text with `at`, and `highlight` infer. The inferred `writeString` accepts a nonempty prefix or suffix and returns a proper string list. Its callers `clearScreen`, `writeAt`, and `moveTo` infer. Read and peek infer, with active reproducers where concrete calls exceed the 20,000-call diagnostic. The accumulator reversal also exceeds that budget. The editor and driver have translation coverage. Reversal stops at the original empty-string tail, and reply centering preserves half-column positions. |
| [boyer.mls](boyer.mls) | The complete theorem prover translates with five repetitions. The symbols are shared instances and the argument comparison succeeds when either list ends, matching the source. Application-symbol checks and forcing the inferred recursive `ONE` lemma check on concrete inputs. Equality and substitution lookup exceed the bounded solver diagnostic. Unification, the remaining delayed lemmas, rewriting, and the driver have translation coverage. The source runtime returns `true`. |
| [boyer2.mls](boyer2.mls) | The complete Lisp parser, rule index, and theorem prover translate with all 106 rules in source order and input `3`. Atom names, basic Lisp projections, truth and delimiter tests, index node construction, and substitution text generation check. The inferred ordering wrapper compares two nonempty names. Composed Lisp projections, recursive equality, token and sublist traversals, index operations, and the complete rule list exceed bounded inference. The source runtime returns `"The term is a tautology"`. |
| [lastpiece.mls](lastpiece.mls) | The full board search, all thirteen piece tables, and the document renderer translate. Concrete checks cover coordinates, orientation changes, map-node sizes, a piece placement, and text fragments. Recursive lookup, composing subtree sizes, and several document reductions exceed bounded inference. Twelve placement tables infer. The `gPiece` table and the list of initial pieces check through translation. The source prints three solutions. |
| [treejoin.mls](treejoin.mls) | The complete join on fixtures `1500.1` and `1500.2` translates. Digit and separator checks, key selection, leaf construction, and formatting a joined row infer. Insertion, lookup, decimal parsing, and rendering the full tree exceed bounded inference. The source needs a larger Node stack to render the fixture join. |
| [sphere.mls](sphere.mls) | The complete 30-by-30 ray trace and exact bigint hash translate. Vector operations, camera parameters, intersection, refraction, and the hash of a concrete colour list check. The inferred hash accepts a nonnumeric colour component and retains a `:breakme`. The ambient selector retains a `:fixme` because the append contract does not expose a nonempty result for `head`. The other selectors, intersection traversal, complete scene, and rendering pipeline check through translation. |
| [sorting.mls](sorting.mls) | All eight sorting passes, the intervening reversals, and the hash of the original fixture translate. The bigint hash, heap index test, and partition definitions infer using the shared contracts. The inferred line splitter rejects a nonempty character list and retains a `:fixme`. Recursive comparison, separator insertion, tree and heap traversals, and the full sorting pipeline check through translation. |
| [rsa.mls](rsa.mls) | The complete fixture encryption and hash translate. Modular exponentiation, plaintext block encoding, and the ciphertext hash check on concrete bigint and character-list inputs. Inference rejects a nonempty input to `collect`. The inferred `encrypt` type accepts only empty plaintext. The decimal character producer composes with `listLen`. The driver retains the original decimal modulus, exponent, fixture path, and argument `0`. |
| [mate.mls](mate.mls) | The complete chess puzzle translates with the original `heathcote3.prob` fixture and argument `0`. Board and move construction, square comparisons and bounds, colour handling, and parsing one square check. Six-way piece dispatch, recursive board access and updates, quicksort, and the lazy fold exceed bounded inference. The comment predicate accepts a nonempty line, and the rank parser infers. Word splitting infers an empty-list input. Optional elimination, force-list refinement, and list search retain active reproducers. The recursive searches and rendering have translation coverage. The translated driver reproduces the original three-move solution and board display. |
| [NofibPrelude.mls](NofibPrelude.mls) | Trusted memoizing delay/force, source string primitives, floating-point addition, multiplication and rendering, and fractional division of integer counts; trusted pair projections, strict equality `===`, `raiseError`, integer multiplication, quotient, and comparisons; inferred `head`, `null_`, `intRem`, `quotRem`, integer `abs`, and floor-division-based `intDiv`/`intMod`. Shared `LNil`/`LCons` constructors, inclusive `enumFromTo` and `enumFromThenTo`, `compose`, `intMax`, `replicate`, `iterate`, `enumFrom`, and `nofibStringToList` infer. Trusted polymorphic `map`, `take`, and `filter` signatures use the constrained recursive list encoding. Their unannotated implementations and the remaining shared traversals are checked through translation in [preludeDeferred.mls](preludeDeferred.mls). The trusted polymorphic `scanl` contract guarantees a nonempty result with the initial accumulator as its first element. The unannotated implementation exhausts a 20,000-call diagnostic. [A fixed-point control for rigid element types](../ctmlRecursiveLists.mls) checks the implementation against the supplied invariant. |
| Unannotated `enumFromTo` | Available in the Nofib prelude and infers a recursive constrained scheme. Endpoint-order, invalid-argument, and implicit recursive-list checks are in `preludeTests.mls` and `ctmlRecursiveProducerInference.mls`. |
| Manual `map` contract | Concrete-list calls check implicitly, including element-changing maps. Composition with inferred `enumFromTo` checks implicitly in `preludeTests.mls`, alongside an explicit range witness. |
| Manual `append` and `listLen` contracts | `append` preserves the common element type, including unions for heterogeneous inputs. `listLen` returns an integer for a proper list of any element type. The unannotated implementations check through translation in [preludeDeferred.mls](preludeDeferred.mls). [Rigid-element fixed-point controls](../ctmlRecursiveLists.mls) check both implementations under supplied invariants. [Caller tests](preludeTests.mls) check composition, result types, and rejection of malformed arguments. The unannotated `concat` has an active `:fixme` because its inferred type accepts only the empty list. |
| Manual `takeLz` and `nofibListToString` contracts | `takeLz` accepts a recursively delayed list and returns a strict list with the same element type. `nofibListToString` joins a proper string list and also models the source `stringListConcat`. [Caller tests](preludeTests.mls) check concrete calls and rejection of a strict tail in a lazy input and non-string elements in the join. Both unannotated implementations check through translation in [preludeDeferred.mls](preludeDeferred.mls). |
| Shared Mandel and text helpers | Floating-point subtraction, division, comparison, integer conversion, maximum, and square root preserve the source operations. Banner's character-code primitive uses an out-of-range sentinel for empty input, which preserves the character classifiers' results. Trusted `mapLz` and `takeLzLz` contracts preserve the delayed recursive boundary. Trusted `concat`, `unlines`, `transpose`, `reverse`, `foldr`, and `foldr1` use proper-list contracts. `foldr1` requires a nonempty input. [Caller tests](preludeTests.mls) check valid calls and rejected arguments. Source implementations have translation coverage or an active inference reproducer in [preludeDeferred.mls](preludeDeferred.mls) and [preludeTests.mls](preludeTests.mls). |
| Manual `foldl`, `break_`, and `leaveLz` contracts | The left fold permits distinct state and element types. The predicate split preserves the element type on both sides and keeps the first match in the suffix. The lazy drop preserves the delayed element type and returns its input handle for nonpositive counts. [Caller tests](preludeTests.mls) check result types and rejected arguments. The unannotated implementations check through translation in [preludeDeferred.mls](preludeDeferred.mls). The source floor quotient and remainder function `divMod` infers. |
| Manual `inList`, `all`, and `sum` contracts | Membership uses strict equality across element types. The universal predicate must return a Boolean for every element, and summation requires a proper integer list. [Caller tests](preludeTests.mls) check result types and reject malformed inputs. The unannotated implementations check through translation in [preludeDeferred.mls](preludeDeferred.mls). |
| Manual `leave`, `leaveWhile`, `listEq`, `atIndex`, and `appendLzLz` contracts | Both drops preserve the element type of a proper list. List equality compares heads using source structural equality. Indexing requires a nonempty proper list and remains partial for out-of-range indices. Lazy append preserves the element type across both deferred inputs. The original traversals have translation coverage or an active reproducer. [Caller tests](preludeTests.mls) check element types and rejected arguments. The nonrecursive `tail` and `splitAt` helpers infer, with `splitAt` using the trusted drop and take contracts. |
| Manual `zip`, `zipWith`, `maximumInt`, `filterLz`, and `listEqBy` contracts | The zip contracts combine proper lists through the shorter length. Maximum requires a nonempty integer list. Lazy filtering preserves the delayed recursive boundary. List equality requires a Boolean comparator and accepts distinct left and right element types. The declarations leave implementation checking to the original unannotated reproducers in [preludeDeferred.mls](preludeDeferred.mls). [Caller tests](preludeTests.mls) check result types and rejected arguments. Pair equality and lazy replication infer and are shared by Knights and Circsim. |
| Shared string comparison, `deleteBy`, `nubBy`, and `ltList` | String ordering follows JavaScript UTF-16 comparison, and string-literal rendering matches the source list renderer. List deletion preserves the element type and removes the first match. Deduplication keeps each first occurrence, and lexicographic comparison uses two Boolean predicates. The original unannotated list traversals check through translation in [preludeDeferred.mls](preludeDeferred.mls). [Caller tests](preludeTests.mls) check concrete calls and reject malformed inputs. |
| Shared source rendering | `renderStringList` infers and formats the bracketed, comma-separated list of quoted strings used by Lambda and Clausify. The trusted `errorMessage` primitive models JavaScript's coercion of an `Error` message to a string. |
| Shared fixture, code-point, bigint, and numeric operations | Trusted reads preserve the original UTF-8 fixture paths. Code-point lookup uses a sentinel for an empty input and the bigint conversion preserves the source TypeError. Bigint arithmetic uses the existing nominal `BigInt` type, with signed remainder and truncating quotient. Floating-point comparison, tangent, power, rounding, and ceiling model the source JavaScript primitives. Mate uses trusted UTF-16 code-unit conversion and JavaScript integer-prefix parsing. The parser returns `Num` to include NaN and large JavaScript numbers. [Caller tests](preludeTests.mls) check result types and reject invalid arguments. |
| List consumers | The focused investigation reproduces timeouts without encoded list signatures, down to unary constructor traversal. `walk`, `copy`, `map`, and dewrapped `length` samples locate the bottleneck in guarded-bound subtyping at the outer `fix` application. Function bodies with supplied recursive calls infer quickly; explicitly typed fixed-point controls pass. See `ctmlRecursiveConsumerInference.mls` and the performance report. Producer/consumer composition remains a separate precision issue. |

**Porting queue** has a CTML counterpart for all 38 source benchmarks. The table records
remaining translation and inference gaps within individual ports.

**Inference follow-up: make algebraically inferred recursive schemes usable without annotations.**
The [consumer performance report](../../../../main/scala/hkmc2/ctml/core/inference/recursive-consumer-performance.md)
localizes a timeout at recursive closure. Bounded CSE diagnostics also exhaust their search
budget before establishing that a traversal's inferred scheme accepts intended calls. Work on
both guarded-constraint search and scheme application, preserving branch refinements, source
recursion, and invalid-call rejection. Definition-only inference is insufficient evidence of
useful recursion support.

The historical
[recursive-summary design](../../../../main/scala/hkmc2/ctml/core/inference/recursive-summary-design.md)
compares FCCTI+-style instantiation sharing with candidate synthesis and body checking. Its
expected-type checking proposal is an alternative experiment, not the preferred implementation
direction. CTML's intended approach is algebraic inference without ordinary function annotations;
any departure needs a general semantic justification beyond improved test counts. The
`ctmlRecursiveSummaryChecking.mls` controls remain useful diagnostics of supplied-contract
checking and do not establish unannotated inference.

No new recursion rule is adopted by this port. Compare proposals on `walk`, `length`, `copy`,
`map`, and the stateful CSE traversals, including actual valid calls and genuine negative controls.
Keep resource exhaustion distinct from subtype refutation. Retain the existing recursive
representation, unannotated benchmarks, and documented library contracts. Substantial solver
design changes remain experiments for review.

CSE keeps the final benchmark input `6` and writes graph triples as nested pairs. Its
`throw Error(message)` branch calls the trusted `raiseError: Str -> Bot` primitive from
`NofibPrelude.mls`. The declaration models the absence of a normal return value. Rendering the
error message and the benchmark output still requires preserving the source list and triple format.
The direct Boolean parser support required by `mif` is checked in `ctmlBooleanConditions.mls`.

SCC preserves the edge and vertex order, both traversals, and input `0`. Its runtime
reference expects `"[[1],[2],[7,5,6],[3,4]]"`. The shared `inList` and `append`
implementations check through translation in [preludeDeferred.mls](preludeDeferred.mls). A separate check of
`LCons(LCons(1, LNil()), LNil()).toString()` fails because CTML's list constructors lack the
source rendering method. SCC and CSE both need the source list format for their entry
points.

LCSS and Integer share the inclusive arithmetic progression. Both drivers need source-format
list rendering. Integer uses unused local bindings to express the source sequencing and shares
the identical traversal body between its two passes. With a temporary traversal contract, its
driver through `testInteger_nofib` infers; the final `toString` lacks a list method.

Cryptarithm's shared `take` and `filter` contracts have caller checks in `preludeTests.mls`.
Their implementations time out during inference. Implicit `take`/range composition and the explicit range witness both check.
The benchmark-local `addj` timeout blocks permutation generation and the entry point; no trusted
signature substitutes for that benchmark computation.

GCD's source translation is complete, including the original `testGcd_nofib(40)` entry point.
The kernel and tuple-mapping stages are active; the maximum and nested-comprehension inference
reproducers exceed the test time limit. These are benchmark-local functions, so the current
library-signature fallback does not replace them with trusted declarations. Continuing active
checking of the entire driver would require addressing those inference costs or agreeing to a
broader signature fallback.

The rigid-element fixed-point map implementation in `ctmlRecursiveLists.mls` remains a separate
checked diagnostic control. Tests of the driver's mapping stages use the manually supplied
library signature and do not establish whole-program inference or runtime equivalence.

The source `enumFromTo(a, b)` includes both endpoints and returns the empty list when `a > b`.
The explicitly typed range example in `ctmlRecursiveLists.mls` is a different, exclusive-endpoint
toy; it is not the source to copy. `max_` is partial on empty lists. The source benchmark calls
`testGcd_nofib(40)`; its MLscript runtime test expects `5021`.

## References

Paths below are relative to this directory:

- [Original benchmark](../../../mlscript-compile/nofib/gcd.mls).
- [Original cryptarithm benchmark](../../../mlscript-compile/nofib/cryptarithm1.mls) and
  [runtime reference](../../nofib/cryptarithm1.mls).
- [CSE source benchmark](../../../mlscript-compile/nofib/cse.mls) and
  [Boolean-condition tests](../ctmlBooleanConditions.mls).
- [Original SCC benchmark](../../../mlscript-compile/nofib/scc.mls) and
  [runtime reference](../../nofib/scc.mls).
- [Original LCSS benchmark](../../../mlscript-compile/nofib/lcss.mls) and
  [runtime reference](../../nofib/lcss.mls).
- [Original Integer benchmark](../../../mlscript-compile/nofib/integer.mls) and
  [runtime reference](../../nofib/integer.mls).
- [Original Life benchmark](../../../mlscript-compile/nofib/life.mls) and
  [runtime reference](../../nofib/life.mls).
- [Original Calendar benchmark](../../../mlscript-compile/nofib/calendar.mls) and
  [runtime reference](../../nofib/calendar.mls).
- [Original Atom benchmark](../../../mlscript-compile/nofib/atom.mls) and
  [runtime reference](../../nofib/atom.mls).
- [Original Mandel benchmark](../../../mlscript-compile/nofib/mandel.mls) and
  [runtime reference](../../nofib/mandel.mls).
- [Original Mandel2 benchmark](../../../mlscript-compile/nofib/mandel2.mls) and
  [runtime reference](../../nofib/mandel2.mls).
- [Original Banner benchmark](../../../mlscript-compile/nofib/banner.mls) and
  [runtime reference](../../nofib/banner.mls).
- [Original Cryptarithm2 benchmark](../../../mlscript-compile/nofib/cryptarithm2.mls) and
  [runtime reference](../../nofib/cryptarithm2.mls).
- [Original Primetest benchmark](../../../mlscript-compile/nofib/primetest.mls) and
  [runtime reference](../../nofib/primetest.mls).
- [Original Power benchmark](../../../mlscript-compile/nofib/power.mls) and
  [runtime reference](../../nofib/power.mls).
- [Original Fish benchmark](../../../mlscript-compile/nofib/fish.mls) and
  [runtime reference](../../nofib/fish.mls).
- [Original Cichelli benchmark](../../../mlscript-compile/nofib/cichelli.mls) and
  [runtime reference](../../nofib/cichelli.mls).
- [Original Puzzle benchmark](../../../mlscript-compile/nofib/puzzle.mls) and
  [runtime reference](../../nofib/puzzle.mls).
- [Original Minimax benchmark](../../../mlscript-compile/nofib/minimax.mls) and
  [runtime reference](../../nofib/minimax.mls).
- [Original Para benchmark](../../../mlscript-compile/nofib/para.mls) and
  [runtime reference](../../nofib/para.mls).
- [Original Knights benchmark](../../../mlscript-compile/nofib/knights.mls) and
  [runtime reference](../../nofib/knights.mls).
- [Original Circsim benchmark](../../../mlscript-compile/nofib/circsim.mls) and
  [runtime reference](../../nofib/circsim.mls).
- [Original Eliza benchmark](../../../mlscript-compile/nofib/eliza.mls) and
  [runtime reference](../../nofib/eliza.mls).
- [Original Constraints benchmark](../../../mlscript-compile/nofib/constraints.mls) and
  [runtime reference](../../nofib/constraints.mls).
- [Original Pretty benchmark](../../../mlscript-compile/nofib/pretty.mls) and
  [runtime reference](../../nofib/pretty.mls).
- [Original Clausify benchmark](../../../mlscript-compile/nofib/clausify.mls) and
  [runtime reference](../../nofib/clausify.mls).
- [Original Awards benchmark](../../../mlscript-compile/nofib/awards.mls) and
  [runtime reference](../../nofib/awards.mls).
- [Original Lambda benchmark](../../../mlscript-compile/nofib/lambda.mls) and
  [runtime reference](../../nofib/lambda.mls).
- [Original ANSI benchmark](../../../mlscript-compile/nofib/ansi.mls) and
  [runtime reference](../../nofib/ansi.mls).
- [Original Boyer benchmark](../../../mlscript-compile/nofib/boyer.mls) and
  [runtime reference](../../nofib/boyer.mls).
- [Original Boyer2 benchmark](../../../mlscript-compile/nofib/boyer2.mls) and
  [runtime reference](../../nofib/boyer2.mls).
- [Original Last Piece benchmark](../../../mlscript-compile/nofib/lastpiece.mls).
- [Original Treejoin benchmark](../../../mlscript-compile/nofib/treejoin.mls) and
  [runtime reference](../../nofib/treejoin.mls).
- [Original Sphere benchmark](../../../mlscript-compile/nofib/sphere.mls) and
  [runtime reference](../../nofib/sphere.mls).
- [Original Sorting benchmark](../../../mlscript-compile/nofib/sorting.mls) and
  [runtime reference](../../nofib/sorting.mls).
- [Original RSA benchmark](../../../mlscript-compile/nofib/rsa.mls) and
  [runtime reference](../../nofib/rsa.mls).
- [Original standard helpers](../../../mlscript-compile/nofib/NofibPrelude.mls).
- [MLscript runtime reference](../../nofib/gcd.mls).
- [Producer inference reproductions](../ctmlRecursiveProducerInference.mls) and
  [isolated subtyping reproductions](../ctmlRecursiveProducerSubtyping.mls).
- [Recursive encoding tests](../ctmlRecursiveEncoding.mls),
  [explicitly typed list controls](../ctmlRecursiveLists.mls), and
  [target-instantiation tests](../ctmlTargetInstantiation.mls).
- [Consumer inference controls](../ctmlRecursiveConsumerInference.mls) and
  [timeout investigation](../../../../main/scala/hkmc2/ctml/core/inference/recursive-consumer-performance.md).
- [Producer-inference investigation](../../../../main/scala/hkmc2/ctml/core/inference/nofib-producer-inference.md).
  Its opening section records the least-carrier rule and its limits.
- [Recursive encoding design and proof report](../../../../main/scala/hkmc2/ctml/core/subtyping/recursive-encoding.md).
  Includes proof obligations and a preexisting cyclic-constraint soundness issue; do not treat
  arbitrary subtyping cycles as proofs or assume every accepted constraint system is sound.

The reports support later checker, paper, and Lean work. They do not require that work to be
completed before adding further Nofib inference tests.
