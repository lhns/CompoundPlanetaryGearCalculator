# ADR 0015 — Topology is the single source of truth; suns are optional per stage

## Status
Accepted. Supersedes the spacing table of ADR 0012 and the `Mode`-keyed parts of ADR 0013/0014.

## Context
`SearchConfig` carried both `mode: Mode` and `topology: Option[Topology]`. The ratio came from the
topology, while spacing, efficiency, the inverse formula, the dedup key, kinematics I/O, labels and exports
read `mode`. The web UI sets `mode = Standard` for every non-canonical layout, so those layouts got Standard's
rules (bug register B2, B3, B5, U6, P5).

Separately, only stage 2's sun could be absent, and the solver handled that with one hard-coded NSS row that
ignored the chosen couplings. The carrier-driven compound is usually built with *no* suns, while ours keeps
real suns. With real suns, `(S+R) % N == 0` is physically required, yet CDCP skipped it.

## Decision
1. **`Topology` is the only description of a gearbox.** `SearchConfig.topology: Topology` (default
   `standardCompound`) replaces `mode`. `CompoundDesign` carries its `topology`. Every rule (spacing,
   stacking, efficiency, solver, inverse, labels, exports, kinematics I/O) is derived from it. `Mode` survives
   only as a CLI/URL alias that maps to the four canonical topologies (`Topology.fromMode`) and as a display
   name via `legacyMode`.
2. **Either sun may be `Absent`.** `BodyRole.Absent` is allowed on `S1` and `S2` (never on a pinned body).
   The canonical CDCP keeps both suns, matching today's model. "Sun-less CDCP" is CDCP with both suns absent.
3. **Planets are explicit unknowns in `RoleSolver`.** Per stage, with ωS, ωP, ωC, ωR:
   - sun mesh (only if the sun is present): `Ts·(ωS − ωC) + Tp·(ωP − ωC) = 0`
   - ring mesh: `Tr·(ωR − ωC) − Tp·(ωP − ωC) = 0`
   - couplings: central welds `ω_a − ω_b = 0`; `PlanetStack` is `ωP1 − ωP2 = 0`
   - pins: Input = ω_in, Fixed = 0

   The unknown count is bodies present plus planets, and the row count matches for every legal topology.
   The NSS special row is gone: NSS is "S2 absent" with ordinary couplings, so couplings now matter there.
   Any remaining degeneracy surfaces as before: a singular system gives `None`, and an idle stage gives the
   decoupled-stage warning (e.g. a sun-less stage that is not stacked only carries its ring through).
4. **Stacking.** A topology is stacked iff either coupling is `PlanetStack`. `PlanetStack` requires the other
   coupling to be `CarrierToCarrier` (a `Topology` invariant; the 8 kinematic chimeras are no longer
   constructible). Stacked ⇒ N1 == N2 and equal centre distance (module matching).
5. **Spacing.** Every stage whose sun is present requires `(S + R) % N == 0`. A sun-less stage has no
   condition of its own; a sun-less stage has no sun mesh, so no sun/ring phase condition (ADR 0012's NSS argument). **Open:** for a
   stacked pinion meshing two rings with no sun at all, a joint phase condition may exist. It is not modelled
   and is noted as a limitation.
6. **Sun-less stages** keep `GearSet(sun = R − 2P)` as a virtual sun so that centre distance and module
   matching stay unchanged. The sun-teeth range and the sun/planet factor do not apply to them, and
   displays and exports show "—".
7. **Efficiency** counts meshes in the power path per present gear: 2 per stage with a sun, 1 per sun-less
   stage. The model is still mesh-count only (power recirculation is ignored; see bug register D4).

## Consequences
- One rule set for every layout. Canonical CDCP now obeys spacing (fewer, assemblable designs). Sun-less
  CDCP is available as a layout of its own.
- Breaking API change inside the repo: `SearchConfig.mode` and `CompoundDesign.mode` are removed, and
  callers use `topology` / `topology.legacyMode`.
- The forward-solver unknown vector changes; `TabularSolver` stays the independent oracle and must be
  extended to absent suns.
