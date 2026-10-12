# ADR 0013 — Generalised role-assignment topology

## Status
Accepted. Written *before* phase D1 lands; updated in place as phases D2-D6 ship.

## Context

The calculator has shipped with four hard-coded topologies — `Standard`, `CDCP`, `NSS`, `SingleStage` — encoded as a closed `Mode` enum. Each mode bakes in a specific (input, output, fixed) assignment plus a specific stage-2 sun presence:

| Mode          | Input    | Output  | Fixed | S2?     |
| :------------ | :------- | :------ | :---- | :------ |
| `Standard`    | Sun 1    | Ring 2  | Ring 1 | yes    |
| `CDCP`        | Carrier  | Ring 2  | Ring 1 | yes    |
| `NSS`         | Sun 1    | Ring 2  | Ring 1 | no     |
| `SingleStage` | Sun      | Carrier | Ring   | —      |

This covers four of the kinematically valid configurations. There are many more: carrier-output reducers, ring-input differentials, sun-fixed overdrives, Wolfrom-style stacked reducers, and so on. Each is a real machine that engineers build. Surfacing only four named modes is artificial.

This ADR sets the data model and forward solver for **any (input, output, fixed) tuple over the bodies present in the gear train**. ADR 0014 (later, when phase D5 lands) covers the inverse-solver dispatcher.

## The kinematic argument: why exactly one Fixed body is required

A single planetary stage has three rotational bodies — sun (S), carrier (C), ring (R) — connected by two meshes:

```
external (sun↔planet):   (ωS − ωC)·Ts + (ωP − ωC)·Tp = 0
internal (planet↔ring):  (ωP − ωC)·Tp − (ωR − ωC)·Tr = 0
```

Adding the two and eliminating ωP gives the Willis equation:

```
Ts·ωS + Tr·ωR − (Ts+Tr)·ωC = 0     (one constraint on three bodies)
```

So a single stage has 3 unknowns − 1 constraint = **2 degrees of freedom**.

A compound (two stages sharing a carrier, with planets rigidly coupled on shared shafts) has 5 unknowns (ωS1, ωC, ωR1, ωS2, ωR2) and three independent constraints — Willis per stage plus the planet-coupling identity ωP1 = ωP2 — giving **2 DOF** again.

For NSS (stage-2 sun absent), the unknowns drop to 4 (ωS1, ωC, ωR1, ωR2) and the constraints to 2 (Willis stage 1 plus the stage-2 planet-ring mesh with ωP substituted from stage 1's mesh), again **2 DOF**.

In every case the system has exactly 2 degrees of freedom. To uniquely determine all body velocities the user must remove both:

- Pin one body's velocity = ω_in (this is the **Input**).
- Pin one body's velocity = 0 (this is the **Fixed** / grounded body).

The body labeled **Output** is *observed*, not pinned — its velocity is whatever the constraints determine. So is every other body (the **Free** ones).

If the user does not specify a Fixed body, the mechanism is under-constrained: it spins as a free differential with no defined output speed. This is a physical property of 2-DOF planetary mechanisms, not a UI choice we can opt out of.

**Exactly one fixed pin is sufficient for both single-stage and compound.** Compound looks like it should need more pins because it has more bodies (5 vs. 3), but the extra bodies are absorbed by the extra Willis equation and the planet-coupling constraint — the system stays 2-DOF. "One Input + one Fixed" pins both DOFs in either case. The remaining bodies are kinematically determined, not free to choose.

## Configuration count

| Body set                                  | Combinations (1 In × 1 Out × 1 Fixed × the rest Free) |
| :---------------------------------------- | :----------------------------------------------------- |
| Single stage (S, C, R)                    | 3 × 2 × 1 = **6**                                      |
| Compound, S2 present (S1, C, R1, S2, R2)  | 5 × 4 × 3 = **60**                                     |
| Compound, NSS (S1, C, R1, R2)             | 4 × 3 × 2 = **24**                                     |
| **Total**                                 | **90**                                                 |

The four legacy modes are four of these 90 points.

## Data model

```scala
enum Body:
  case S1, C, R1, S2, R2
  // P1, P2 are kinematic derivatives, not user-selectable.

enum BodyRole:
  case Input, Output, Fixed, Free, Absent
  // Absent is permitted only on S2 (the NSS case).

final case class Topology(stages: Int, roles: Map[Body, BodyRole]):
  // construction-time invariants:
  //   stages ∈ {1, 2}
  //   every body that is "present" has a role
  //   exactly one Input, one Output, one Fixed
  //   Absent permitted only on S2
```

`Topology` enforces *structural* validity (the four `require` clauses above). It does not enforce *solvability* — a few rare role tuples produce a singular linear system; those surface as `None` from the forward solver.

## Forward solver — one linear system, not 90 closed forms

The forward problem ("given a topology and tooth counts, compute ω_out/ω_in") reduces to a small linear system that is *uniform* across all 90 tuples — only the rows that pin Input and Fixed change, plus the optional planet-coupling row when S2 is present.

The system, with unknowns `(ωS1, ωC, ωR1, ωS2, ωR2)`:

```
Willis stage 1:   Ts1·ωS1 + Tr1·ωR1 − (Ts1+Tr1)·ωC = 0
Willis stage 2:   Ts2·ωS2 + Tr2·ωR2 − (Ts2+Tr2)·ωC = 0    (when S2 present)
Planet coupling:  Ts1·Tp2·ωS1 + (Ts2·Tp1 − Ts1·Tp2)·ωC − Ts2·Tp1·ωS2 = 0   (S2 present)
ω(Input) = 1                                                (always)
ω(Fixed) = 0                                                (always)
```

For NSS (S2 absent) the stage-2 Willis is dropped and the planet-coupling row becomes a single combined constraint:

```
Tp2·Ts1·ωS1 − (Tp1·Tr2 + Tp2·Ts1)·ωC + Tp1·Tr2·ωR2 = 0
```

with ωS2 removed from the unknowns. For single-stage, the system reduces to 3 unknowns and the single Willis row.

We solve via partial-pivoting Gauss elimination in a tiny hand-rolled `LinAlg.solve` (≤ 5×5 dense). The forward direction is ≈ 100 floating-point ops per call — comparable to the legacy closed-form formulas — but works for *every* tuple with no per-tuple code.

This is a key architectural choice: forward gets one solver, not 90 closed forms. The 200-sample property tests in `RoleSolverSpec` verify agreement with the four legacy closed forms.

## Inverse solver — dispatcher, not unified

The inverse problem ("given a topology, stage 1, and a target ratio, find stage-2 tooth counts close to target") *does* benefit from closed forms — for example, `Mode.Standard` has the closed form `Tr2 = T·Ts1·Tr1·Tp2 / (Tp1·(Ts1·(T−1) − Tr1))`. Solving the forward system 1000× during a targeted search would be slower than evaluating the closed form 1000×.

The plan is a per-tuple dispatcher with brute-force fallback (ADR 0014):

- D5 ships closed forms for the four legacy tuples.
- Any non-legacy tuple falls back to brute-force pair enumeration, scored by `|ratio − target|`, bounded by the existing `ComputeCap = 10_000`.
- New closed forms can be added one at a time as users request them.

## Kinematics generalisation

`Kinematics.at(d, phi)` becomes a thin wrapper around `RoleSolver.velocities(topology, s1, s2, omegaIn = phi)`, returning the existing `BodyAngles` record. The per-mode branching in `Kinematics.at` disappears.

This is what unlocks every topology in the animated schematic for free: D3 lets the existing rendering display any of the 90 configurations correctly because the body velocities are computed uniformly.

## UI

- Stages segmented control: Single / Compound.
- Three native `<select>` dropdowns: Input, Output, Fixed. Each filters out bodies already chosen by the other two.
- "Stage 2 has its own sun gear" toggle (Compound only): toggles S2's role between `Free` (when toggle on) and `Absent` (NSS).
- A derived label below the pickers translates back to a legacy mode name when applicable, otherwise descriptive: *"Configured: sun-driven compound, ring 2 out, ring 1 fixed (Standard)"* or *"Configured: carrier-input compound, sun 1 out, ring 2 fixed (no legacy name)"*.
- Interactive schematic (D6): clicking a gear opens a small role-picker popover. Two-way bound with the form selects.

## Migration

Phasing (one shippable PR per phase):

1. **D1** — Data model (this ADR + `Body`, `BodyRole`, `Topology` rewrite + `TopologySpec`).
2. **D2** — Forward solver (`LinAlg`, `RoleSolver`, `LinAlgSpec`, `RoleSolverSpec`). `Compounder.compoundRatio` becomes a thin adapter for the legacy `Mode` API.
3. **D3** — Kinematics generalisation (`Kinematics.at` reads `d.topology`, delegates to `RoleSolver`).
4. **D4** — UI role pickers replace the existing segmented-control topology widget.
5. **D5** — Inverse-solver dispatcher + brute-force fallback (ADR 0014).
6. **D6** — Interactive schematic role-picker popovers.

Between D1 and D5, `Mode` continues to exist for adapter purposes. After D5, no production code (outside `Topology.fromMode` / `toLegacyMode`) references `Mode`. It is then either kept solely as an internal cache key or deleted — decision deferred to D5.

## Test invariants

`core/src/test/scala/cpgc/constraints/TopologySpec.scala` (D1):
- `Topology.fromMode ∘ toLegacyMode == identity` for the four legacy modes.
- Construction rejects two-Input, two-Output, two-Fixed, no-Input, no-Output, no-Fixed topologies.
- `Absent` rejected on bodies other than S2.
- `Body.present` returns the correct set for each `(stages, s2Present)`.
- `Topology.legacyMode` returns the right legacy `Mode` for each canonical tuple, `None` for novel tuples.

`core/src/test/scala/cpgc/search/LinAlgSpec.scala` (D2):
- Solver produces `|A·x − b| < 1e-9` for 200 random non-singular 5×5 systems.
- Solver returns `None` for singular systems.

`core/src/test/scala/cpgc/search/RoleSolverSpec.scala` (D2):
- Forward solver matches the legacy `Compounder.compoundRatio` for each of the four legacy modes across 200 random gear sets, within `1e-9`.
- Forward solver agrees with three hand-derived ratios for novel tuples (carrier-input + ring1-output + ring2-fixed; sun1-input + carrier-output + ring1-fixed; ring1-input + sun1-output + carrier-fixed) within `1e-9`.

`core/src/test/scala/cpgc/kinematics/RoleKinematicsSpec.scala` (D3):
- Every body angle in `BodyAngles` agrees with the legacy `Kinematics.at` for each of the four legacy modes across 100 random configs × 10 phi values, within `1e-9`.
- Input body angle equals φ exactly across all 90 tuples.
- Fixed body angle equals 0 exactly across all 90 tuples.
- Planet coupling ωP1 = ωP2 within `1e-9` for all compound-with-S2 topologies.

`core/src/test/scala/cpgc/search/RoleInverseSolverSpec.scala` (D5):
- Each registered closed form's ratio is within `1e-3` of the brute-force best for 100 random `(stage1, target)` pairs.
- Brute-force fallback yields ≥ 1 result for a deliberately uncovered tuple.
- Dispatcher routes to the registered closed form for each registered tuple (instrumented counter).

## Tradeoffs

- **Forward solver via Gauss elimination, not 90 closed forms.** Forward is fast enough either way; one solver is dramatically less code to maintain. The four legacy closed forms remain as the property-test reference, not as runtime code.
- **Inverse solver kept as a dispatcher with brute-force fallback.** Inverse is genuinely closed-form-faster (single-pass over P2 vs nested loops over (P2, S2)). Keeping the four legacy closed forms preserves the existing targeted-search performance; new tuples opt into closed-form gradually.
- **`StageGears` ADT split (deferred in ADR 0012) stays deferred.** The role-assignment refactor doesn't force it. The NSS virtual-S2 storage trick continues to work.

## Future

- Phase D6 click-driven role popovers.
- Per-tuple URL deep-linking (`?topology=...`).
- User-defined named topologies (favourites).
- Animated transitions when the user changes roles, showing the body-velocity flips in real time.
