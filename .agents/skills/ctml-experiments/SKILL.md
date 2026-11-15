---
name: ctml-experiments
description: Coordinate parallel CTML research experiments, manage isolated agent branches, critically evaluate results, and report recommendations. Use for delegated CTML experimentation campaigns, not ordinary single-task checker edits.
---

# Coordinate CTML experiments

Act as a research coordinator for CTML. Dispatch focused investigations, coordinate Git and
resources, challenge results in the broader design, and recommend what deserves further work.
The priorities are recursive-type subtyping, inference for recursive functions, controlling
subtyping complexity, and faithful unannotated Nofib ports. Choose work by its expected information
value and dependencies, rather than treating these as four independent implementation tasks.

## Authority and session bounds

- Use at most five concurrent subagents across the entire delegation tree. Keep allocation under
  coordinator control; workers must request a slot before delegating further. Count agents doing
  active work, including background commands; release a slot only after that work has stopped.
  Completed agents retained solely for their history do not occupy the research concurrency limit.
- Create checkouts, experiment branches, commits, and experimental combinations as needed.
  **Never merge, cherry-pick, rebase, reset, or otherwise advance `ctml`.** The user alone decides
  what to merge into it. Deliver candidate branches and recommendations. Do not push or publish
  results unless separately authorized.
- Read the budget supplied for this session: time, tokens, both, or explicitly unlimited.
  Do not carry an unlimited allowance into another session or infer it from the day of the week
  or presumed account quota. A changed budget or stop instruction takes effect immediately.
- If no budget is supplied, perform one bounded investigation round and report; do not silently
  start an indefinite campaign. A round has a concrete research question and a review checkpoint,
  not an arbitrary promise to close all CTML holes. Select and announce a finite time allowance
  for that round before dispatch, reserving part of it for the report.
- With an unlimited budget, continue useful investigation and iteration while progress is
  justified. Unlimited does not require repeating failed approaches or inventing work after the
  objective is achieved. Stop on user instruction, an exhausted applicable limit, achievement,
  or a documented impasse that needs user input. Leave a report in each case.
- For finite budgets, allocate room for baseline work, integration checks on experiment branches,
  critical review, and the final report. Monitor available usage and elapsed time; do not claim
  precise token accounting if the environment does not expose it. Do not promise an enforceable
  token cap without an accounting mechanism. Use checkpoints and disclose that limitation.
- Creating or editing this skill does not itself start an experimentation campaign.

## Establish the research state

Locate the active repository; the main-machine path is
`/media/Data/HKUST/Research/mlscript`. Read its applicable `AGENTS.md` and relevant local guidance.
Inspect status, existing worktrees, experiment branch tips and ancestry before assigning work.
Preserve user changes and existing branches. Record exact commits rather than relying on moving
branch names.

Useful sources, relative to the repository root:

- `hkmc2/shared/src/main/scala/hkmc2/ctml/`: checker, local design notes, and experiment reports.
- `hkmc2/shared/src/test/mlscript/ctml/`: focused examples and known limitations.
- `hkmc2/shared/src/test/mlscript/ctml/nofib/README.md`: port decisions and current handoff.
- Sibling paper: `/media/Data/HKUST/Research/ctml/latex`.
- Sibling mechanization: `/media/Data/HKUST/Research/ctml/lean`.

Read relevant branch-local reports using their recorded revision. Treat reported measurements,
proof coverage, and proposed next steps as evidence to inspect, not current verified facts or
binding design decisions. The user's latest research direction takes precedence over older
recommendations. The paper, Lean development, and Scala implementation are all evolving; explain
discrepancies and suggest corrections where appropriate instead of forcing one to mirror another.

Establish test baselines in an isolated checkout before refactoring. Distinguish recursive producer
instantiation, consumer contract inference, and guarded-bound search costs when the evidence
supports that separation. Reassess it when new evidence shows a different decomposition.

Maintain a durable Markdown ledger on a coordinator-owned experiment branch or in a campaign
directory outside the user's working tree. Record its path in updates and the final report.
Keep hypotheses, assignments, base/tip commits, checkout paths, evidence locations, budgets,
decisions, and next questions current. Keep evolving state here, not in this skill.

## Assign and coordinate investigations

Give each worker a brief containing:

- A concrete question or hypothesis, its motivation, and what evidence could refute it.
- The exact starting commit, exclusive checkout and branch, permitted scope, and dependencies.
- Relevant design constraints and guidance, especially soundness, algebraic inference, and
  preservation of unannotated programs.
- Expected deliverables: argument, code or counterexample, validation, limitations, and commits.
- A budget/checkpoint and stopping conditions proportionate to the question.

Assign implementation, proof work, counterexample search, performance measurement, or independent
review as useful. Do not fill all slots merely because they exist. Have workers report conceptual
obstacles early, before extending a design with successive special cases. Communicate discoveries
between related investigations; avoid duplicating work on the same underlying bottleneck.

The coordinator must examine consequential code and arguments, not simply combine worker summaries.
For a promising substantial proposal, use another agent to challenge its assumptions and search
for counterexamples when practical. Give that reviewer the proposal and raw evidence without
dictating the desired verdict. Remain responsible for the final assessment.

## Git and execution isolation

Use one exclusive worktree or local clone per active worker, each on a uniquely named
`experiment/...` branch. Choose worktrees when sharing Git metadata is convenient and clones when
stronger isolation helps. Agents share a filesystem and may inherit the coordinator's working
directory: supply absolute checkout paths and require explicit working directories for commands.
Do not let workers edit or switch the user's main checkout.

The coordinator owns checkout creation and branch allocation. Workers may commit their assigned
work, but must not alter other workers' branches or checkouts. Shared worktree Git metadata needs
coordinated branch operations. Never rewrite or delete pre-existing experiment history as cleanup.

Pin upstream experiment tips before branching from them. Combine results only in a dedicated
experiment checkout, record the input commits, resolve semantic as well as textual conflicts,
and validate the combined behavior. A clean merge does not establish compatible invariants.
Keep component experiments available for comparison and attribution.

Keep build outputs, generated golden files, and temporary probes within their owning checkout.
Coordinate concurrent sbt/Lean builds to avoid memory contention and misleading timings. Measure
performance in isolation with comparable settings and warm-up. Stop only servers and processes
owned by the campaign. Preserve useful branches, reports, and unfinished work on interruption;
do not delete evidence to make a workspace appear clean.

## Evaluate research quality

Evaluate the following dimensions separately. Do not combine them into a score that permits
passing tests to compensate for unsoundness.

**Soundness and proof scope.** State the semantic rule or invariant that justifies the change,
including scope, variable identity, residual obligations, and assumptions. A timeout or abstract
revisit is not a proof of a subtype judgment. Check negative examples and counterexamples to the
proposed rule. Distinguish a theorem about a transformation from verification of the Scala
implementation or the entire solver. For Lean work, record its revision, assumptions, unfinished
proofs or added axioms, and actual verification commands. Never present an admitted claim as a
completed proof. Known-unsound exploratory artifacts must be clearly labeled and cannot be
recommended for adoption.

**Generality and inference.** Prefer algebraic inference and general constraint transformations
that preserve useful solution families. Explain how the approach interacts with relevant unions,
intersections, negation, polymorphism, constrained types, polarities, and scopes. Identify the
supported fragment and what happens outside it; generality need not mean solving everything at
once. Distinguish a principled design argument from a formal principality theorem.

Bidirectional checking, supplied invariants, or recognizing structural shapes can be useful
experiments or controls. They are not justified as CTML's general solution merely by improving
examples. Explain any departure from annotation-free algebraic inference and why it is necessary.
An inferred scheme is useful only if intended valid calls work and relevant invalid calls fail.

**Cost and scalability.** Identify the source of improvement with controlled comparisons or
ablations where feasible. Measure solver calls, formula/type growth, allocations, or elapsed time
as appropriate. Distinguish finite observations from asymptotic or termination claims. Verify
that performance gains are not just weaker inferred types, discarded obligations, early aborts,
changed budgets, or disabled tests. A finite instance graph does not itself bound formula growth.

**Architectural cost.** Account for new concepts, interactions, special cases, and duplicated
mechanisms. Prefer a coherent refactor or replacement when it reduces overall complexity.
Do not protect existing architecture merely to minimize the diff, or accumulate machinery merely
because each addition fixes another test. Follow repository conventions and keep shared upstream
infrastructure changes justified and small.

**Tradeoffs.** Soundness is non-negotiable for adoption; completeness and precision regressions
can be acceptable. For example, losing the typing of omega may be worthwhile for substantially
better recursive inference. Identify the lost capability, affected examples, reason, and broader
benefit, then recommend a tradeoff for the user's judgment. Do not silently redefine success.

## Tests, Nofib, and supporting research

Follow the repository's test instructions and compare against each experiment's exact baseline.
CTML diff tests rewrite golden output; inspect those changes, not only exit status. Report newly
resolved expected failures, new failures, existing failures, inconclusive searches, and disabled
timeouts separately. Do not count a newly marked failure as an inference improvement.
Run focused checks during iteration and the required broader checks before recommending a result.
Preserve reproducers for unresolved bugs with the repository's failure markers. Respect its file
timeout rules and do not commit unrelated generated output.

For Nofib, read the porting guide and original source. Preserve algorithms, inputs, evaluation
structure, and ordinary unannotated definitions. Follow agreed representation choices and the
documented library-signature fallback. Count translation completeness, successful inference,
checking against trusted signatures, and runtime validation separately. Do not rewrite benchmarks
or weaken contracts to conceal checker limitations. Use real ports to challenge proposed rules,
and small reductions to explain failures. This campaign authorizes solver experiments; older
port-only deferrals do not prohibit them. Other local decisions remain applicable unless the
user's current instructions supersede them.

Use primary literature, local or online, and Lean when they can resolve a concrete uncertainty.
Record sources and distinguish imported results from extensions to CTML. Work in isolated copies
for changes to the sibling Lean project; leave the main paper and mechanization untouched unless
the user separately requests edits there. Do not require mechanization of every prototype, but
match the strength of recommendations to the available evidence.

## Review, iterate, and report

At each checkpoint, choose whether to continue, redirect, combine, retain as a limited result,
or discontinue an experiment. Record why. A refutation, a clear limitation, or removal of an
unjustified approach is useful progress. Avoid repeating an experiment without a changed
hypothesis or new evidence. Revisit priorities across all four goals after consequential results.

Keep the user informed of findings, changes of direction, budget pressure, and unresolved choices.
When stopped, halt further dispatch, ask active agents to stop and return their current state,
and cancel running campaign-owned validation where possible. Collect available evidence without
launching additional validation. Mark unfinished or unverified work explicitly. Record any
process or task that could not be stopped.

Produce a detailed final report, even if no implementation is recommended. Include:

- The campaign objective, budget interpretation, baseline revisions, and scope actually covered.
- Each agent's question, approach, branch and commits, results, evidence, and limitations.
- Reproducible commands and locations of code, proofs, logs, measurements, and counterexamples.
- Test and performance changes with their controls, including regressions and incomplete checks.
- The coordinator's own assessment of soundness, generality, scalability, and architectural cost.
- Recommended candidates, promising follow-ups, rejected directions, and unresolved decisions.
- The exact branches/commits the user could inspect for merging, with dependencies and caveats.
- Remaining checkouts, running processes, and unfinished work needed for a clean handoff.

Distinguish observed facts, arguments, conjectures, and recommendations. Link the durable report
and summarize the main conclusions in the final response. Do not merge into `ctml`.
