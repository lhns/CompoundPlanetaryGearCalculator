# Correctness verification strategy

How the calculator establishes, and quantifies, confidence that the numbers it prints are
right. It claims measured confidence, not proof. Findings are tracked in
[`bug-register.md`](bug-register.md); the assembly rules are derived in
[`assembly-conditions.md`](assembly-conditions.md). The harness lives in
`core/src/test/scala/cpgc/verify/` (plus `core/.jvm/src/test/` for JVM-only specs).

## 1. Principle: no calculation is trusted on its own

Each result must agree with at least one independent method that shares no code with
it. The oracles:

| Oracle | What it is | Used against |
|---|---|---|
| Textbook / hand-derived goldens | Willis equation `Ns·ωs + Nr·ωr = (Ns+Nr)·ωc`; the three single-stage forms (ring fixed `1 + Nr/Ns`, sun fixed `1 + Ns/Nr`, carrier fixed `−Nr/Ns`); hand-worked compounds | `RoleSolver`, `Kinematics`, efficiency, modules |
| `TabularSolver` (N-version) | Superposition-method solver written without `LinAlg`, row assembly or body indexing. Covers single stages and shared-carrier compounds with any second coupling, with or without absent suns (virtual sun `Tr − 2·Tp`) | `RoleSolver.ratio` |
| `AssemblyOracle` | Exact tooth-phase model: can N identical (or individually phased) planets be installed? | the production assembly rule (ADR 0018) |
| Reference differential | Fixtures from the author's 2019 Python program (`tools/reference/`) | the full design set for Standard, NSS, sun-less CDCP |

Plus a self-consistency check: after every solve, the velocity vector is substituted back
into every constraint row (`‖A·x − b‖ < 1e-9`).

## 2. Harness inventory

**Contract, completeness, cross-path** (run for every topology in `ContractSupport.cases`:
the four canonical ones plus novel couplings, sun-less variants and single-stage role tuples):

| Spec | Checks |
|---|---|
| `DesignContractSpec` | Every design from `search` and the engine satisfies the intended rules: ratio equals the forward solver, `S + 2P = R`, tooth bounds, planets fit, spacing/assembly rule, module matching, stacked `N1 == N2`, efficiency, single-stage shape |
| `CompletenessSpec` | Top-K errors from `search`, the engine and `best()` equal an exhaustive reference computed from the rules here, so nothing is missed or mis-ranked |
| `CrossPathSpec` | `best()` and the engine only return designs `search` emits; identical-stage pairs, negative ratios and `N1 ≠ N2` (non-stacked) are found |
| `InputRobustnessSpec` | Invalid OD, module, ranges and targets give no results, never an exception |

**Solver and kinematics**: `TabularSolverSpec` (RoleSolver vs the oracle across random
gear sets and topologies), `search/RoleSolverSpec` (goldens, textbook forms, totality,
self-consistency), `CoupledForwardSpec`, `IndependentCarrierSpec`, `AbsentSunSpec`
(sun-less layouts), `LinAlgSpec`, `DecoupledStageSpec` (degeneracy warnings),
`kinematics/RoleKinematicsSpec`, `KinematicsSpec`, `verify/AnimationConsistencySpec`
(the animated angles reproduce the ratio, for every UI preset in `Topology`).

**Geometry and assembly**: `AssemblyConditionSpec` (production rule vs `AssemblyOracle`,
including the ADR 0018 counterexamples), `ModuleMatchingSpec`, `GeometrySpec`,
`PlanetSpacingSpec`, `render/GearGeometrySpec` (mesh-phase offsets), `constraints/TopologySpec`
(invariants), `EfficiencySpec`.

**Search engine**: `search/SearchEngineSpec` (bounded top-K, ranking, chunked stepping,
truncation, `phasedPinions`).

**Reference differential** (JVM): `ReferenceDifferentialSpec` compares teeth sets and
ratios with the fixtures. Known reference defects (R1–R4 in the bug register) are encoded
explicitly; the literal-reference NSS cases are `.fail`.

**Export and UI**: `export/ExportSpec`, `OpenScadSpec`, `ExportLocaleSpec`;
`web/UiLogicSpec` (URL parsing, topology editing, form validation, presets, animation
per preset).

**Diagnostics** (excluded from `sbt test`, tagged `diagnostic`):
`PerformanceDiag` (timings per preset) and `TopologyEnumerationDiag` (full topology design
space; writes `target/diag/`). Run:

    sbt "project coreJVM" "set Test / testOptions := Seq()" "testOnly cpgc.verify.PerformanceDiag cpgc.verify.TopologyEnumerationDiag"

## 3. Confidence matrix

O = independent oracle, G = golden values, P = property/random sweep, E = edge cases enumerated and tested.

| Component | O | G | P | E | Notes |
|---|:-:|:-:|:-:|:-:|---|
| `RoleSolver` forward ratio | ✓ | ✓ | ✓ | ✓ | `TabularSolver` covers all shared-carrier couplings and absent suns. Independent-carrier layouts (Ravigneaux, Simpson) have goldens and self-consistency only |
| `LinAlg` | | ✓ | ✓ | ✓ | Substrate of the oracle comparison; residual checks substitute |
| Search (enumeration, top-K, ranking) | ✓ | | ✓ | ✓ | Exhaustive reference in `CompletenessSpec`; reference program for the canonical layouts |
| Assembly / spacing rule | ✓ | ✓ | ✓ | ✓ | `AssemblyOracle`; ADR 0018 |
| Module matching | | ✓ | ✓ | ✓ | |
| Efficiency | | ✓ | ✓ | ✓ | Model is mesh-count only (bug register D4) |
| Kinematics / animation | ✓ | | ✓ | | Ratio agreement through `RoleSolver` |
| Planet fit (`PlanetSpacing`) | | ✓ | | | Exact chord plus clearance (ADR 0016); reference uses a stricter approximation (R3) |
| Involute geometry (render only) | | ✓ | ✓ | | Does not affect ratios |
| Exports | | ✓ | ✓ | ✓ | OpenSCAD tooth phasing is checked against a mesh model, not a real CAD run |

Known limits: the efficiency model (D4); a stacked pinion meshing two rings with no sun at
all may have a joint phase condition that is not modelled (ADR 0015 §5); independent-carrier
topologies lack a second solver.

## 4. How to extend

A new coupling, topology, formula or search rule counts as verified when:

1. Its rule is stated once in `ContractSupport` (spec side) and the production code is
   checked against it by the contract and completeness specs.
2. It has an independent oracle (`TabularSolver` coverage, a textbook form or the
   assembly oracle), or the gap is listed in section 3.
3. Edge cases are tested; real bugs go in the bug register with a failing test first.
4. Any new preset is added to `Topology` (the UI and `AnimationConsistencySpec` both read it).

A change that lowers a component's coverage is a regression and should be called out in review.
