# ADR 0002 — Pure functional core

## Status
Accepted.

## Context
The search space is "enumerate tooth combinations, filter geometrically, pair them into compounds, score by error to target." This is a natural fit for lazy streams over immutable data.

## Decision
- The `core` module contains only pure functions and immutable case classes / opaque types.
- Enumeration uses `LazyList[GearSet]` so consumers materialize only what they need.
- Top-K ranking uses a bounded `PriorityQueue` internally to `Scorer.topPerTarget`, but the mutable state never escapes the function — the input is a `LazyList` and the output is a `Map[Ratio, Vector[Scored]]`.
- All I/O (stdout, file writes, DOM) lives in `cli` and `web`.

## Consequences
- Trivially testable: feed a config, get a deterministic sequence.
- Trivially cross-compilable: no JVM-only APIs in `core`.
- `LazyList` keeps memory bounded for large sweeps when the caller only requests the head.
