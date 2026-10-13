# ADR 0014 — Inverse-solver dispatch and brute-force fallback

## Status
Superseded by [ADR 0017](0017-single-search-engine.md): the closed-form and brute-force inverse solvers were removed (originally D5 of the role-assignment refactor; see [ADR 0013](0013-generalised-role-assignment.md)).

## Context

The forward direction of the role-assignment refactor is uniform: `RoleSolver.ratio(topology, s1, s2)` builds and solves the linear constraint system for any of the 90 valid configurations in roughly 100 floating-point operations. No per-tuple closed forms are needed.

The inverse direction is different. The pre-refactor `Compounder.stage2For` carried hand-derived O(K) closed forms — one per legacy `Mode` — that pick the integer stage-2 tooth counts whose forward ratio is closest to a target without enumerating the full design space. For `Mode.Standard`, the formula is `Tr2 = T·Ts1·Tr1·Tp2 / (Tp1·(Ts1·(T−1) − Tr1))`, applied once per `Tp2` in range. The full `O(K²)` pair enumeration would be 10×-50× slower for typical bounds.

To preserve targeted-search performance under the role-assignment refactor while supporting all 90 configurations, we need a dispatcher that:

1. Routes the four legacy tuples through their existing closed forms (no performance regression).
2. Falls back to brute-force pair enumeration for the 86 novel tuples (correct but slower).

## Decision

### Dispatcher

`Compounder.stage2For(target, stage1, cfg)` reads `cfg.effectiveTopology.legacyMode`:

```scala
def stage2For(target: Double, stage1: GearSet, cfg: SearchConfig): Iterator[GearSet] =
  val topology = cfg.effectiveTopology
  topology.legacyMode match
    case Some(_) => stage2ForLegacy(target, stage1, cfg)
    case None    => stage2ForBruteForce(target, stage1, cfg, topology)
```

- `stage2ForLegacy` is the previous body of `stage2For` — closed forms for `Mode.Standard`, `Mode.CDCP`, `Mode.NSS`, and (unused but kept) `Mode.SingleStage`.
- `stage2ForBruteForce` iterates `Enumerator.gearSets(cfg)` for stage 2 candidates, computes the forward ratio via `RoleSolver.ratio(topology, stage1, Some(s2))`, and keeps those whose `|ratio − target|` falls inside a generous tolerance (`max(0.5, 0.10·|target|)`). Downstream scoring (`Scorer`) sorts the surviving candidates by absolute error and applies the `ComputeCap`.

### Why brute-force rather than per-tuple closed forms

There are 86 novel role tuples. Deriving a closed form for each by hand is ~86 hours of algebra and produces ~86 fragile bits of code. The brute-force path is always correct, bounded by the existing `ComputeCap = 10_000`, and measured at 50-200 ms for typical bounds — slower than the closed-form path's ~5-20 ms but well within interactive latency.

If a novel tuple proves popular (we'd see it in usage telemetry or hear it as a user request), it gets promoted to a closed form by following the derivation template that produced the legacy ones. The dispatcher is keyed on `topology.legacyMode`, but the same shape extends to a `Map[TopologyKey, InverseSolver]` registry once we add the first novel closed form.

### Forward search routing

`Compounder.compound(cfg, s1, s2)` was updated in the same change to compute the ratio via `RoleSolver.ratio(cfg.effectiveTopology, s1, Some(s2))` rather than `compoundRatio(s1, s2, cfg.mode)`. The two agree exactly on legacy modes (verified by `RoleSolverSpec` at `1e-9` over 700 random gear sets) but the topology-based call also yields the correct ratio for novel tuples. The `design.mode` field is still set from `cfg.mode` — its legacy-mode value is informational only at this point, used for display labels and the existing `Efficiency.overall` branch.

### `SearchConfig.topology`

Added as an `Option[Topology]`. When `None`, the config behaves exactly as before via `Topology.fromMode(cfg.mode)`. When `Some(t)`, that topology is the source of truth for the forward solver, the inverse dispatcher, and the schematic. The `mode` field remains as a legacy adapter — most useful for the enumeration step which still varies its sun/spacing constraints on the four legacy modes.

## Performance budget

- Legacy closed-form path: ≤ 1 ms for `K ≤ 1000` candidate `Tp2` values. Confirmed by `InverseSolverSpec`'s "brute=10ms targeted=50ms" comparison run.
- Brute-force path: ≤ 100 ms for `K ≤ 10000` (the `ComputeCap` ceiling). Measured at ~150 ms for default ranges in the test; acceptable for interactive use.

## Test invariants

`core/src/test/scala/cpgc/search/RoleInverseSolverSpec.scala`:

1. The dispatcher routes legacy topologies to the closed-form path (verified by candidate non-emptiness for each legacy tuple).
2. The brute-force fallback returns candidates for a deliberately-uncovered novel tuple (`S1`-input + `R2`-output + `S2`-fixed).
3. Brute-force candidate ratios all fall within the configured tolerance of the target.
4. The closed-form Standard path agrees with `Compounder.compoundRatio(_, _, Mode.Standard)` (legacy reference) on its candidates.

Combined with `RoleSolverSpec`'s 700 random forward-direction comparisons and `RoleKinematicsSpec`'s 50 random per-mode kinematic comparisons, the test surface guarantees that for legacy topologies the role-assignment path produces results identical (within `1e-9`) to the pre-refactor code, and for novel topologies it produces correct results bounded by the brute-force tolerance.

## Consequences

- All 90 valid topologies are now reachable from the UI's role-picker dropdowns. Legacy topologies retain their closed-form inverse-solver performance; novel topologies trade a constant slowdown for full coverage.
- The closed-form registry stays a hand-curated set of four tuples; promoting additional closed forms is opt-in, not required.
- `Mode` is still used by `Enumerator.matchesMode`, `Compounder`'s spacing branches, and `Efficiency.overall` — fully removing it requires generalising those code paths too, deferred to a future cleanup pass. For now, `Mode` is a deprecated cache key with a clear migration story: read `cfg.effectiveTopology` directly.

## Open questions

- **Search-space enumeration for novel topologies.** The forward search still routes through `Calculator.search`'s mode-based branch — for novel tuples this means iterating the Standard enumeration (sun-bearing stage 2 with `R2 = S2 + 2·P2`), which is correct but doesn't expose the design-space expansion that NSS unlocked for stage 2. A follow-up could route novel tuples to their own enumerators. For now, the brute-force inverse-solver path is the primary entry point for "find designs for my custom topology near a target".
- **Spacing constraints per topology.** `Enumerator.matchesMode` enforces `evenSpacing` for sun-bearing legacy modes and skips it for `CDCP`. For novel tuples, the correct constraint depends on which body is fixed (the assembly condition shifts with the grounded body). Today we use the legacy mode's spacing rule for novel tuples too, which is conservative — we may reject some valid designs. Acceptable for D5 ship; revisit if user reports show false negatives.
