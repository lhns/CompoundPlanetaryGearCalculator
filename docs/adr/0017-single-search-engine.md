# ADR 0017 — One exhaustive search engine with a bounded top-K

## Status
Accepted. Supersedes ADR 0014 (inverse-solver dispatch) and the result handling in ADR 0010.

## Context
The CLI (`Calculator.searchTargeted`, `best`) and the web (`SearchSteps` plus a raw 10 000-design cap) ran
different searches. The closed-form inverse solvers solved only the signed target, tried a single rounded
`Tr2`, forced `N2 = N1` and filtered differently from the forward path, so targeted results missed better
designs (bug register B4). `Scorer.topPerTarget` evicted the best candidate (B1). The web sorted only the first
10 000 designs found (B9). With planets as explicit unknowns (ADR 0015) the ratio is in general a ratio of
quadratics in `Tr2`, so no exact closed-form inverse exists for arbitrary couplings.

## Decision
- One step iterator (the `SearchSteps` shape) enumerates **all** stage pairs from per-topology stage pools,
  including identical pairs and, for non-stacked topologies, `N1 ≠ N2`. The CLI drains it, the web runs it in
  chunks, and `Calculator.search`/`best` are thin wrappers.
- Results go into one bounded accumulator: top-K by the selected key (best-fit error per target, or ratio,
  efficiency or simplicity), deduplicated by both stages' teeth, both planet counts and the topology. A
  truncated run (web time cap) reports "best so far, truncated".
- Closed-form inverse solvers are removed. An accelerator may be re-added later only if
  `CompletenessSpec` passes with no expected failures.
- Performance budget at default ranges: browser under ~2 s, JVM CLI under ~5 s. Levers, in this order: an
  allocation-free solver hot path; cheap rejects before solving (spacing, stacked-N mismatch); pairing only
  equal N for stacked topologies.

## Consequences
- Targeted and untargeted searches return the same designs; only ranking differs.
- Wide ranges cost time, not correctness; the UI states when results are truncated.
