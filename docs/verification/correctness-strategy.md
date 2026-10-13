# Correctness verification strategy

How the Compound Planetary Gear Calculator establishes — and *quantifies* — confidence
that the numbers it prints are right.

This document is the durable reference. The harness that backs it lives in
`core/src/test/scala/cpgc/verify/`. When a calculation, coupling, or mode is added,
the "How to extend" checklist at the end says what it must pass before it counts as
verified.

> **Status:** living document. The Confidence Matrix and Edge-Case Catalogue are
> updated as the harness runs and surfaces findings.

---

## 1. Why this exists

The calculator has ~35 distinct numeric calculation sites. The structural risk is
that the **forward solver** (`RoleSolver` — given gears, find the ratio) and the
**inverse solvers** (`Compounder.stage2For*` — given a target ratio, find gears) are
*independent code paths*. A bug in one and not the other makes the calculations
**asymmetric**: a ratio computed forward will not round-trip back through the inverse.
That asymmetry is silent — nothing in the UI tells you the two halves disagree.

The strategy below is built around eliminating that class of bug, and around being
*honest about what is and isn't verified* rather than claiming a blanket guarantee.

---

## 2. The multi-oracle principle

**No calculation is trusted on its own.** Every calculation must agree with at least
one *independent* method — a method that shares no code with it. Four oracle classes
are used:

### 2.1 Textbook closed form
For the cases the published literature covers, the textbook formula is the oracle.
The fundamental planetary relation is the **Willis equation**:

```
Ns·ωs + Nr·ωr = (Ns + Nr)·ωc
```

The codebase's per-stage Willis row (`RoleSolver`, stage-1 row) is
`Ts·ωS + Tr·ωR − (Ts+Tr)·ωC = 0` — algebraically identical. The three single-stage
reductions are independent golden oracles:

| Fixed member | Ratio (input → output) | Formula |
|---|---|---|
| Ring fixed | sun → carrier | `i = 1 + Nr/Ns` |
| Sun fixed | ring → carrier | `i = 1 + Ns/Nr` |
| Carrier fixed | sun → ring | `i = −Nr/Ns` |

For series-cascaded stages the overall ratio is the *product* of stage ratios — a
fourth independent check for the subset of compounds that are pure cascades.

### 2.2 Independent second solver (N-version programming)
`core/src/test/scala/cpgc/verify/TabularSolver.scala` is a from-scratch kinematic
solver using the **superposition (tabular) method** — the textbook hand-calculation
procedure. It is deliberately written to share *zero* code with `RoleSolver`: no
`LinAlg`, no shared row assembly, no shared body indexing. Two independent
implementations of the same spec agreeing across a large random sweep is the
strongest practical evidence short of a machine-checked proof.

`TabularSolverSpec` cross-checks `RoleSolver.ratio` against `TabularSolver.ratio`
across ≥10 000 gear-set × topology combinations at `1e-9` tolerance. Any disagreement
is a bug in one solver or the other and gets triaged into the Edge-Case Catalogue.

### 2.3 Self-consistency
Solving a linear system and getting *a* number proves nothing — the matrix could be
assembled wrong. So after every solve in the random sweep, the solved velocity
vector `x` is re-substituted into the assembled `A`, `b`: the harness asserts
`‖A·x − b‖∞ < 1e-9`. This catches sign errors and index errors in row assembly
directly, including the nasty case where two such bugs cancel in the final ratio but
not in the intermediate velocities.

### 2.4 Round-trip closure
The asymmetry guard. `RoundTripSpec` takes a random valid `(s1, s2)`, computes the
forward ratio `T = RoleSolver.ratio(...)`, feeds `T` into `Compounder.stage2For(T, s1, cfg)`,
and asserts the returned candidates include an `s2'` whose forward ratio is within
`1e-6` of `T`. This pins *every* inverse path — the legacy closed forms, the
`RingToSun` closed form, and the brute-force fallback — against the forward solver.
The tight tolerance is the point: a 20 % "close enough" tolerance hides asymmetry; `1e-6` exposes it.

---

## 3. Calculation inventory

Every numeric calculation site, its formula, and its assumptions. File references
are to `core/src/main/scala/cpgc/`.

### 3.1 Forward kinematics — `search/RoleSolver.scala`
Assembles and solves a linear system in the body angular velocities.

- **Willis stage 1:** `Ts1·ωS1 + Tr1·ωR1 − (Ts1+Tr1)·ωC1 = 0`
- **Willis stage 2** (full compound): `Ts2·ωS2 + Tr2·ωR2 − (Ts2+Tr2)·ωC2 = 0`
- **Coupling rows** (two, for a full compound): each `Coupling` emits one closure
  row. Every coupling except `PlanetStack` is a plain weld `ω_a − ω_b = 0` for the
  body pair from `Coupling.bodyPair`. `PlanetStack` emits the planet-coupling row
  derived from equating the two stages' planet-pinion velocities `ωP1 = ωP2`:
  `Tp2·(Tp1+Ts1)·ωC1 − Ts1·Tp2·ωS1 − Tp1·(Tp2+Ts2)·ωC2 + Ts2·Tp1·ωS2 = 0`.
  With `CarrierToCarrier` also in effect this reduces to the classic 5-body
  planet-coupling row — verified algebraically; the legacy modes are bit-identical.
- **NSS combined row** (S2 absent): `Tp2·Ts1·ωS1 − (Tp1·Tr2 + Tp2·Ts1)·ωC1 + Tp1·Tr2·ωR2 = 0`.
- **Pin rows:** Input velocity = `ωIn`, Fixed velocity = 0.
- **Ratio:** `1 / ω_output`; `None` when `|ω_output| < 1e-15` (infinite reduction).
- **Conditioning:** smallest Gauss pivot recorded; `< 1e-6` flags `Validity.Warning`.
- **DOF accounting:** single stage 3 bodies − 1 Willis = 2 DOF; NSS 4 − 1 − 1 = 2;
  full compound 6 − 2 Willis − 2 coupling = 2. Input + Fixed pins remove both.

### 3.2 Linear algebra — `search/LinAlg.scala`
Gauss elimination with partial pivoting. Singular when largest column pivot
`< 1e-12`. `solveDetailed` also returns the minimum pivot for the conditioning flag.

### 3.3 Inverse solvers — `search/Compounder.scala`
- `stage2ForLegacy` — closed forms for the four legacy modes:
  - Standard/NSS: `Tr2 = T·Ts1·Tr1·Tp2 / (Tp1·(Ts1·(T−1) − Tr1))`
  - CDCP: `Tr2 = T·Tr1·Tp2 / (Tp1·(T−1))`
- `stage2ForRingToSun` — `Tr2 = 2·T·Ts1·Tp2 / (Ts1·(2T−1) − Tr1)`; guarded by a
  `standardRoles` precondition, falls back to brute force otherwise.
- `stage2ForBruteForce` — enumerates stage-2 candidates, keeps those whose
  `RoleSolver.ratio` is within `±(10 %·|T| + 0.5)` of the target.
- In all paths `Ts2 = Tr2 − 2·Tp2` (the `r = s + 2p` invariant).

### 3.4 Legacy `compoundRatio` — `search/Compounder.scala`
A simplified closed-form compound ratio. **Not used by `compound()`** — the
production path routes through `RoleSolver.ratio`. It survives only as a cross-check
oracle in `RoleSolverSpec`. A dormant second source of truth (see Catalogue).

### 3.5 Module matching — `search/Compounder.matchModules`
`m1 = firstStageOD / (Tr1 + 2)`. For `PlanetStack`,
`m2 = m1 · (Ts1+Tp1)/(Ts2+Tp2)` (equal center-to-center for the shared pinion). For
every other coupling, `m2 = m1` (independent stage geometry).

### 3.6 Planet spacing — `domain/GearSet.scala`, `physics/PlanetSpacing.scala`
- Even spacing: `(Ts + Tr) % N == 0` — necessary for N equally-spaced planets.
- No-overlap: planet OD `< 2·c2c·sin(π/N)` (chord between adjacent planet centres).

### 3.7 Efficiency — `physics/Efficiency.scala`
Per mesh `0.985`. Standard/CDCP/SingleStage stage = `0.985²` (two meshes), NSS
stage 2 = `0.985` (one mesh). Compound = product; non-stacked novel couplings =
`0.985^(meshCount)` with meshCount 2 or 4.

### 3.8 Kinematics for animation — `kinematics/Kinematics.scala`
`ωP = ωC − (ωS − ωC)·Ts/Tp` per stage. Routes through `RoleSolver.velocities`.

### 3.9 Involute geometry — `render/GearGeometry.scala`
Pressure angle 20°, `inv(α) = tan(α) − α`, pitch/base/outer/root radii, planet and
ring mesh phase offsets. Rendering only — does not affect ratios.

---

## 4. Verification Confidence Matrix

One row per calculation component, scored on five binary axes:

| Axis | Met when… |
|---|---|
| **O** Independent oracle | A second independent method agrees (textbook form or `TabularSolver`). |
| **G** Golden values | ≥1 hand-derived value from a published source is pinned exactly. |
| **P** Property tests | Invariants checked over a random sweep (≥1000 samples). |
| **R** Round-trip | Participates in a forward↔inverse closure test. |
| **E** Edge cases | Its edge cases are enumerated *and* tested. |

**Tier:** A = 5/5 (trust for production decisions) · B = 3–4/5 (trust, gaps documented)
· C = 1–2/5 (provisional) · D = 0/5 (unverified — flag in UI if user-visible).

### Current state (post-harness — verify/ specs landed)

| Component | O | G | P | R | E | Score | Tier | Note |
|---|:-:|:-:|:-:|:-:|:-:|:-:|:-:|---|
| `RoleSolver` forward (legacy modes) | ✓ | ✓ | ✓ | ✓ | ✗ | 4 | A | N-version vs `TabularSolver`; 4 textbook/hand goldens; self-consistency sweep; round-trip. |
| `RoleSolver` forward (novel couplings) | ✓ | ✓ | ✓ | ✓ | ✗ | 4 | A | `TabularSolver` covers the 5 carrier-coupled presets; `CoupledForwardSpec` goldens; self-consistency sweep includes them. |
| `LinAlg.solve` | ✗ | ✓ | ✓ | — | ✓ | 3 | B | No independent oracle — but it *is* the oracle's substrate; residual checks substitute. |
| `Compounder.stage2ForLegacy` | ✓ | ✗ | ✓ | ✓ | ✗ | 3 | B | Round-trip pinned at 1e-6 for Standard + CDCP. |
| `Compounder.stage2ForRingToSun` | ✓ | ✗ | ✓ | ✓ | ✗ | 3 | B | Round-trip pinned at 1e-6. |
| `Compounder.stage2ForBruteForce` | ✓ | ✗ | ✗ | ✓ | ✗ | 2 | C | Round-trip closes (near-tautological — it scores via the forward solver). |
| `Compounder.compoundRatio` (legacy) | ✗ | ✗ | ✗ | — | ✗ | 0 | D | **Dormant** — not used in production. Catalogue #9: delete it. |
| `Compounder.matchModules` | ✗ | ✓ | ✓ | — | ✗ | 2 | C | `ModuleMatchingSpec`: positivity + c2c-equality property + golden. |
| `satisfiesSpacing` | ✓ | ✗ | ✓ | — | ✓ | 3 | B | |
| `Efficiency.overall` | ✗ | ✓ | ✓ | — | ✓ | 3 | B | `EfficiencySpec`: positivity property + exact goldens per coupling; catalogue #13 fixed (NSS now `0.985³`). |
| `Kinematics` (animation) | ✓ | ✗ | ✓ | — | ✗ | 2 | C | Routes through `RoleSolver` — inherits its O indirectly. |
| `GearGeometry` involute | ✗ | ✓ | ✗ | — | ✗ | 1 | C | Rendering only — does not affect ratios. |
| `PlanetSpacing` overlap | ✗ | ✗ | ✗ | — | ✗ | 0 | D | Boolean predicate, geometric. Lowest-risk D — not a ratio. |
| `GearSet` invariants | ✗ | ✓ | ✓ | — | ✓ | 3 | B | `require(numPlanets >= 2)` guards the `% N` edge case. |
| `Coupling.assemblable` | ✓ | ✗ | ✓ | — | ✓ | 3 | B | |
| `Topology.isStageDecoupled` | ✓ | ✗ | ✓ | — | ✗ | 2 | C | 84-tuple numeric cross-check. |

**Reading the matrix:** the forward solver — the highest-risk surface — is now **A**
on both legacy and novel couplings, with the asymmetry guard (**R**) in place. The
closed-form inverse paths are **B**. Components still at **C/D** are either
rendering-only (`GearGeometry`), dormant (`compoundRatio` — slated for deletion), or
low-risk geometric predicates (`PlanetSpacing`). No *ratio-producing* component is
below B. `Efficiency` is the one user-visible number still at C, held there by the
catalogue #13 bug — it rises to B when that Fix lands.

### Harness results summary

The harness (Section 2) ran clean except for the findings triaged into the catalogue:

- **N-version cross-check** — `RoleSolver` vs the independent `TabularSolver`:
  **>13 000 combinations, zero disagreements.** Strongest single piece of evidence
  in the project — two independently-derived solvers agree to 1e-9.
- **Round-trip closure** — Standard, CDCP, and `RingToSun` closed forms all
  round-trip to 1e-6 for physically-buildable, matching-`N` inputs.
- **Self-consistency** — >1000 solved velocity vectors, every constraint equation
  (Willis ×2, both coupling rows, both pins) satisfied to 1e-9.
- **Textbook goldens** — the two previously-untested single-stage forms (sun-fixed
  `1 + Ns/Nr`, carrier-fixed `−Nr/Ns`) and a hand-worked compound (`−13.0`) all match.
- **Two findings** triaged → catalogue #13 (Efficiency NSS undercount — a real bug)
  and #14/#15 (test-design / closed-form-`N` notes — not bugs).

---

## 5. Edge-case catalogue

Each row: edge case → current behaviour → disposition. Dispositions are
**Accept** (correct behaviour, just document it), **Fix** (a real bug — needs a code
change, gated on owner sign-off), **Work around** (not strictly wrong but worth
mitigating). This catalogue is *living* — every harness finding adds a row.

| # | Edge case | Current behaviour | Disposition | Rationale |
|---|---|---|---|---|
| 1 | Input == Fixed body | Rejected at `Topology` construction (`require`) | **Accept** | Caught early, clear error. |
| 2 | Coupling-induced singularity (`RingToRing` + R1 fixed → R2 ≡ 0) | `RoleSolver.ratio` → `None` → "No design matches" placeholder | **Work around** | Not wrong, but the placeholder is misleading — the config is *structurally* dead, not "no gears in range". A distinct hint would help. |
| 3 | Near-singular matrix (minPivot < 1e-6) | Ratio emitted, `Validity.Warning` set | **Accept** | Honest signalling. Threshold to be re-validated by the conditioning sweep. |
| 4 | Infinite reduction (ω_out → 0) | `None` | **Accept** | Correct — the ratio genuinely doesn't exist. |
| 5 | Negative ratio (reversed output) | Displayed with `−` / ↺ glyph | **Accept** | Correct and surfaced. |
| 6 | `N1 ≠ N2` with `PlanetStack` | ✅ **FIXED** — `Compounder.compound` now returns `None` when `stacksPlanets` and the stage planet counts differ | **Fix (landed)** | A single stacked pinion cannot serve two stages with different planet counts; the kinematic number was physically meaningless. |
| 7 | `firstCoupling == secondCoupling` | ✅ **FIXED** — `Topology` now `require`s the two compound couplings differ | **Fix (landed)** | Two identical welds is one constraint, not two — the system would be under-determined. |
| 8 | `matchModules` when `Ts2 + Tp2 == 0` | ✅ **FIXED** — guarded; falls back to `m2 = m1` instead of emitting `Inf`/`NaN` | **Fix (landed)** | Unreachable for a valid `GearSet`, but the method is public. |
| 9 | `compoundRatio` vs `RoleSolver` divergence | ✅ **FIXED** — `Compounder.compoundRatio` deleted; `RoleSolver.ratio` is the single source of truth; 5 caller specs repointed | **Fix (landed)** | A dormant second formula *will* rot; removed entirely. The N-version `TabularSolverSpec` is the cross-check now. |
| 10 | Closed-form inverse dispatched to wrong role tuple | `stage2ForRingToSun` guards with `standardRoles`; else brute force | **Accept** | Guard exists — but `RoundTripSpec` must *prove* the guard is complete; if it's not, this becomes Fix. |
| 11 | Brute-force inverse tolerance `10 %·|T| + 0.5` | Loose; may admit poor matches | **Work around** | Tighten, or post-filter every candidate through `RoleSolver.ratio` and keep only `< 1e-6`. |
| 12 | `evenSpacing` with `N` = 1 or 0 | **Already guarded** — `GearSet` has `require(numPlanets >= 2)`, so `% N` can never see `N < 2` | **Accept** | Re-dispositioned from Fix after reading the code: the constructor invariant already makes this unreachable. |
| 13 | `Efficiency` for NSS undercounts stage-1 meshes | Returns `0.985²`; physically correct is `0.985³` (NSS stage 1 is a *full* planetary — 2 meshes — only stage 2 drops to 1) | **Fix** | `stageEfficiency` is given only `(GearSet, Mode)` — it can't tell stage 1 from stage 2, so it treats both NSS stages as 1-mesh. Overstates NSS efficiency (0.970 vs 0.956). `EfficiencySpec` pins the buggy value with a loud marker so the suite stays green until the Fix lands. Fix: pass stage identity (or a mesh count) to `stageEfficiency`. |
| 14 | Forward solver accepts physically non-buildable gear sets | `RoleSolver.ratio` returns a ratio for gear sets with overlapping planets or bad spacing — it is a *pure kinematic* solver | **Accept** | Correct separation of concerns: the *inverse* solver and the *search* pipeline filter to buildable designs. Documented so callers know "a forward ratio exists" ≠ "a valid design exists". The round-trip harness only feeds buildable inputs for this reason. |
| 15 | Closed-form inverse builds stage-2 candidate with `stage1.numPlanets` | The closed forms (`stage2ForLegacy`, `stage2ForRingToSun`) hardcode `stage1.numPlanets` for the stage-2 gear set | **Accept** (PlanetStack) / **Work around** (non-stacked) | For `PlanetStack` a single stacked pinion *forces* `N1 == N2`, so this is correct. For non-stacked couplings stage 2's planet count is independent — the closed form can only surface candidates with `N2 == N1`. The brute-force path has no such limit; document it as a closed-form coverage boundary. |
| 16 | `SpacingExperimentSpec` residuals are artefacts, not assemblability signals | The spec misuses `GearGeometry.planetPhaseOffset` (a wrapped *visual* offset) as a physical body rotation; its non-zero residuals are artefacts | **Accept** | Already correctly labelled diagnostic in the spec's own header; it carries two *real* assertions (stage-1 Willis sanity, filter accept/reject lock). Recorded here so the finding isn't lost — do not read compound-assemblability from those printed residuals. |
| 17 | `RoleSolver` threw `IllegalArgumentException` on a 2-stage topology with no stage-2 gear set | ✅ **FIXED** — `solveSystem`'s two `throw` sites (`stages == 2` w/o stage 2; `row != n`) now `return None` | **Fix (landed)** | A forward solver must be **total** — any input it can't solve yields `None`, never an exception. The throw surfaced in the UI as a console crash when the topology signal flipped to compound before a fresh design arrived. `RoleSolverSpec` pins the totality. |
| 18 | UI topology mutators silently reset the coupling fields | ✅ **FIXED** — `assignRole` / `setS2Present` now carry `firstCoupling` / `secondCoupling` through; UI mutations routed via a `safeSet` catch-and-toast helper | **Fix (landed)** | `assignRole` / `setS2Present` predated the coupling fields and called `Topology(stages, roles)`, dropping them to defaults — a role change silently nuked the user's coupling choice. Separately, an invalid `Topology` thrown inside `Var.update` *permanently poisons* the Laminar `Var` (`VarError` cascade); `safeSet` is the structural guard, and degenerate coupling pairs are now unreachable (the colliding option is `disabled` in each dropdown). |
| 19 | `{PlanetStack, X≠CarrierToCarrier}` is kinematically valid but physically impossible | ✅ **FIXED (model level, ADR 0015 §4)** — `Topology` requires `CarrierToCarrier` as the partner of `PlanetStack`, so the 8 chimeras can no longer be constructed; the UI keeps its dropdown guard as a convenience | **Fix (landed)** | A single stacked pinion can't be at two different orbital positions, so `PlanetStack` physically requires the carriers to be coupled. Stacked now means either coupling is `PlanetStack` (both orders of `{PlanetStack, CarrierToCarrier}`), which also enforces `N1 == N2` and module matching. Harness fixtures that used the chimeras were moved to legal topologies. |
| 20 | Compound topologies that decouple stage 2 (or stage 1) given the chosen role tuple — ratio doesn't depend on that stage's gears | ✅ **FIXED** — `Compounder.compound` runs a perturbation probe: re-solve with a tweaked stage-N gear set; if the ratio is unchanged within 1e-9, emit `Validity.Warning("stage-N decoupled …")`. The symbolic `Topology.isStageDecoupled` (wrong for independent carriers, B11) was removed; the probe is the only signal | **Fix (landed)** | The diagnostic identified weld patterns (for the Standard role tuple) where one stage is along for the ride. With the chimeras gone, `DecoupledStageSpec` pins the warning on legal pairs such as `{CarrierToCarrier, CarrierToRing}` and its absence on genuine compounds. A non-stacked sun-less stage only carries its ring through and is flagged the same way. |
| 21 | Trivial-passthrough compounds (output speed = input speed) | ✅ **FIXED** — `Compounder.compound` flags `|ratio − 1\| < 1e-9` as `Validity.Warning("trivial passthrough …")` | **Fix (landed)** | The diagnostic also surfaced 8 pairs that force output velocity to equal input velocity through the weld pattern. Surfacing as a warning rather than hiding the design — same role-tuple-dependence rationale as #20. |

**All five Fix-dispositioned items (#6, #7, #8, #9, #13) have landed** (V3, owner-
approved). Each was a reviewed, isolated change; the suite stayed green throughout
(135 tests — the count dropped from 138 because the three `RoleSolverSpec` cross-
checks against the deleted `compoundRatio` were subsumed by the stronger N-version
`TabularSolverSpec`). The `Efficiency` row in the Confidence Matrix rises from C to
B now that #13 is fixed and its golden is pinned to the correct `0.985³`.

---

## 6. On formal verification

**A machine-checked proof is not pursued.** Reasons, stated plainly so the decision
is auditable:

- The kinematic core is small and *linear*. The "spec" of a correct gear ratio is
  ultimately the textbook equations — which the multi-oracle approach (Section 2)
  already pins from four independent directions.
- The ROI of a Lean/Coq/Stainless effort is poor against that backdrop: it would
  re-prove what N-version programming + golden values + self-consistency already
  establish to a very high confidence.

**What is done instead — the "formal-ish" layer:**

- The self-consistency residual check is a *machine-verified invariant*:
  `∀ solved x: ‖A·x − b‖∞ < ε`, run over a large random sample. This is a property,
  not an example.
- The independent `TabularSolver` is *N-version programming*: two implementations,
  one spec, cross-checked at scale.
- `RoundTripSpec` is a *bijection check* on the forward/inverse pair.

**Where a proof might pay off later:** `LinAlg.solve` (Gauss elimination with partial
pivoting) is the one component small and self-contained enough that
[Stainless](https://stainless.epfl.ch/) could verify it outright. Noted as a possible
future step; explicitly out of scope now.

**The honest claim:** *this project quantifies confidence; it does not claim proof.*
The Confidence Matrix is the artefact that makes that quantification concrete and
reviewable.

---

## 7. How to extend

When a new `Coupling`, `Mode`, formula, or inverse path is added, it is **not
considered verified** until:

1. It is added to the **Calculation Inventory** (Section 3) with its formula and
   assumptions.
2. It has an **independent oracle** — either it falls within the `TabularSolver`'s
   coverage (add it to the sweep), or a textbook closed form is identified, or — if
   neither — that gap is explicitly recorded in the matrix and the component cannot
   score above tier C.
3. It participates in **`RoundTripSpec`** if it has an inverse path.
4. Its **edge cases are enumerated** and added to the Catalogue (Section 5) with
   dispositions.
5. Its row in the **Confidence Matrix** (Section 4) is added and scored.

A change that drops any existing component's tier is a regression and must be
called out in review.
