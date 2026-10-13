# ADR 0010 — Search termination, inverse-solver, and the 10 000 compute cap

## Status
Accepted; the inverse solver and result handling are superseded by [ADR 0017](0017-single-search-engine.md).

## Context

Three closely-related questions about the search algorithm needed pinning down once the inverse-solver fast path landed:

1. How does the algorithm know it has finished — is the search ever truncated for "still running" reasons?
2. Can we miss designs by capping the result set?
3. Should we keep a hard safety cap, and where should it sit?

## Decision

### Termination

The search is **exhaustive over the configured tooth-count bounds**. There is no notion of an unbounded search space:

- `cpgc.search.Enumerator.gearSets` iterates every `(numPlanets, sun, planet)` combination in the configured ranges and yields a finite `LazyList[GearSet]`.
- For untargeted mode, `Calculator.search` pairs every two valid stage-1 sets (full `O(K²)` enumeration).
- For targeted mode, `Calculator.searchTargeted` consumes the same single-stage stream and, for each stage-1 candidate × each target × each `Tp2` in range, algebraically derives the closest `Tr2` via `Compounder.stage2For`.

When the finite cross-product is consumed, the `LazyList` ends. That's why a 1 000 000-entry cap returns the same 542 designs as a 1 000-entry cap for the same bounds — there simply aren't more designs to find.

### Missed designs

- **Untargeted (`Calculator.search`)** is fully exhaustive within bounds. A cap only truncates if the cap is smaller than the natural search-space size.
- **Targeted (`Calculator.searchTargeted`)** is exhaustive over the *near-target* subset. For every `(stage1, Tp2)` it picks the integer `Tr2` algebraically closest to the target ratio; other `Tr2` values have strictly worse error and are intentionally skipped. So we don't miss anything for "top-K by target error", but we *do* skip designs that aren't near any target — which is the whole point of targeted search.

The underlying mathematics is classical Diophantine inversion / continued-fraction-style approximation: for any real target `T` the integer pairs `(p, q)` closest to `p/q = T` can be discovered analytically without exhaustively sweeping the (p, q) grid. See `Compounder.stage2For` for the closed-form solve.

### The 10 000 compute cap

`App.ComputeCap = 10_000` is a fixed safety ceiling on the result-set size, applied at the `LazyList.take` boundary inside `App.runSearch`. Rationale:

- Typical user bounds (sun 8–80, planet 8–20, ring 40–200, planets 3–4) yield `O(100)` to `O(1000)` valid designs. The cap never binds.
- Pathological bounds (e.g. sun 2–500, planet 2–500, planets 2–10) produce `O(K²) ≈ 10⁶+` pair-attempts and can freeze a browser tab; the cap clips at 10 000 and the rest of the bound is silently dropped.
- The cap is **hidden and constant**, not a user knob. The user explicitly asked not to surface it ("choose a fixed cap of 10 000 not configurable"). The right thing for power users in that situation is to narrow their tooth-count ranges, not to enlarge the cap.

### Display vs compute separation

The web UI separates two concerns that the early `topK` field conflated:

- **Compute cap** (`ComputeCap = 10_000`, hidden) — how many results to materialise and rank.
- **Display count** (`displayCount = Var(20)`, paginated) — how many of those to show in the table. A "Show more" button below the table extends `displayCount` by 20 per click. CSV / JSON exports still dump the full computed set, not the visible window.

## Consequences

- No user-tunable cap field on the form — fewer UI knobs, less confusion.
- For typical workflows the user sees the top 20 designs sorted by their chosen criterion (best fit, ratio, efficiency, simplicity) and paginates through more on demand.
- Power users who genuinely need more than 10 000 results can edit the constant or use the CLI's explicit `--top` flag, which has different ergonomics.
- The targeted-mode invariant — "the inverse solver returns the best-by-target-error designs, ranked exhaustively within bounds" — is now documented and tested (`InverseSolverSpec`).
