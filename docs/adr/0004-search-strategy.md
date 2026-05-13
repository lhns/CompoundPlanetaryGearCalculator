# ADR 0004 — Enumerate-then-pair, top-K per target

## Status
Accepted.

## Context
A compound design is a pair of single-stage gear sets. The naive nested-loop approach (`for sun1, planet1, sun2, planet2`) explodes combinatorially. Most pairs are also invalid before they are paired (planet overlap, ring out of range), so re-checking them inside the inner loop is wasteful.

## Decision
- **Enumerate first:** `Enumerator.gearSets` produces a `LazyList[GearSet]` of single-stage candidates that pass every per-stage filter. We materialize this list once per `Calculator.search`.
- **Pair second:** `Compounder.compound` is called only on ordered pairs of already-validated stages.
- **Top-K per target:** when the caller supplies target ratios, `Scorer.topPerTarget` keeps only `k` best per target via a bounded heap — memory is `O(targets * k)` regardless of stream length.

## Consequences
- The set of valid single-stage candidates is typically small (low hundreds), so squaring it for pairing remains tractable.
- For unbounded sweeps (no targets), users get a raw `LazyList[CompoundDesign]` and decide how many to pull.
- The pair loop is `O(|stages|²)`. For very large tooth bounds we may revisit this with banding/indexing on `ringPlanetRatio`.
