# Bug register

Findings from the systematic review of `wip/role-topology-refactor`. Each
confirmed bug with an automated reproduction is pinned by tests marked munit
`.fail` and named after its ID. When a fix lands, munit reports those tests as
unexpectedly passing; remove the `.fail` marker then.

Sources: the verification harness (`core/src/test/scala/cpgc/verify/`), the
reference differential (`core/.jvm/src/test/.../ReferenceDifferentialSpec`,
`tools/reference/`), UI-logic tests (`web/src/test/`), a headless browser smoke
run, and three code reviews.

Status: **confirmed-test** (failing test exists) · **confirmed-run** (reproduced
in the CLI or browser) · **confirmed-read** (verified by reading or arithmetic,
no test yet) · **design** (needs a decision) · **limitation** (documented, not a bug).

## What is right
- The ratio formulas for Standard, CDCP and NSS agree exactly (1e-9, ratio and
  both modules) with the reference program across 12 fixtures. The forward
  solver agrees with the independent tabular solver.
- For the four legacy layouts, the full search (`Calculator.search`) emits
  exactly the reference design set.

## Search and ranking (core)
| ID | Bug | Effect | Status |
|---|---|---|---|
| B1 | `Scorer.topPerTarget` heap is ordered backwards and evicts the best candidate | CLI `--target` and `Calculator.best`: only #1 is right; ranks 2..k are arbitrary early designs (53.12 → 53.119, 56, 56) | **fixed** (was: confirmed-test, confirmed-run) |
| B4 | CLI (`searchTargeted`) and web (`SearchSteps`) are separate search implementations with different inverse paths. The inverse solves only the signed target, so negative-ratio designs are missed. Only `round(Tr2)` is tried. Stage 2 always gets `N2 = N1`. The CDCP/NSS inverse skips the sun range that the enumerator applies. Identical-stage pairs are excluded. The brute-force fallback has a ±10 % window | Targeted results miss better designs and contain designs outside the bounds; `searchTargeted ⊄ search` | **fixed** (was: confirmed-test) |
| B6 | `canonicalKey` ignores the planet count; repeated or close targets give duplicates | Distinct N=3/N=4 designs are merged; duplicate rows | **fixed** (was: confirmed-test) |
| B7 | `firstStageOD <= 0` throws in `Module.apply` instead of returning no results | CLI stack trace; web "Search failed" toast | **fixed** (was: confirmed-test, confirmed-run) |
| B8 | CLI without a target prints the first N enumerated designs, unsorted | `--top` is not a top-K | **fixed** (was: confirmed-read) |
| B9 | Web search keeps the first 10 000 designs in enumeration order (plus a 5 s time cap), then sorts | Best designs can fall outside the cap; truncated runs vary between runs | core fixed (engine top-K); web wiring in progress |
| B10 | `math.round(x).toInt` overflow and absolute singularity thresholds can leak near-infinite ratios | Rare garbage rows | open (low): closed-form overflow gone with the inverse solvers; absolute singularity threshold remains |

## Mode vs topology (core model)
| ID | Bug | Effect | Status |
|---|---|---|---|
| B2 | `Mode` and `Topology` are two sources of truth. The web sets `mode = Standard` for every novel layout, and spacing, efficiency, `design.mode`, the inverse formula, the dedup key, `Kinematics.io`, the labels and the exports all read the mode | Novel layouts get Standard rules and labels. Example: NSS + RingToRing shows 9 642 designs labelled "sun-driven"; an NSS-like layout's efficiency is 0.985⁴ instead of 0.985³ | **fixed** (was: confirmed-test, confirmed-run) |
| B3 | A single-stage layout with custom roles goes down the compound path; `Compounder.single` ignores the roles | Every row gets an unrelated stage 2 attached | **fixed** (was: confirmed-test, confirmed-run) |
| B5 | Stacked-planet logic reads only `secondCoupling`; the `RingToSun` closed form ignores `firstCoupling` | (PlanetStack, CarrierToCarrier) emits N1≠N2 designs with m2=m1 (wrong centre distance); SunToCarrier+RingToSun targeted search returns nothing | **fixed** (was: confirmed-test) |
| B11 | `isStageDecoupled` treats C1 as the carrier of both stages | Wrong "single-stage equivalent" hint for independent-carrier layouts | **fixed** (was: confirmed-read (low)) |

## Web UI
| ID | Bug | Effect | Status |
|---|---|---|---|
| U1 | "Stage 2 has its own sun" toggle builds an invalid topology (C2 role left over or missing) | The toggle always fails with an error toast | confirmed-test, confirmed-run |
| U2 | A teeth minimum < 2 (e.g. while typing) or a non-finite target throws outside the search's try/catch | UI stuck on "Searching…" | confirmed-test, confirmed-run |
| U3 | A target of `53,12` is split into the targets 53 and 12 | Searches for 12:1 | confirmed-test, confirmed-run |
| U4 | OD 0 or negative → "Search failed" plus a misleading "No design matches"; stale rows remain | | confirmed-test, confirmed-run |
| U5 | URL parsing: `mode=` matched by substring, no percent-decoding, single-value ranges ignored, state never written back to the URL | | confirmed-test |
| U6 | NSS: coupling pickers are enabled, but the solver ignores couplings when there is no second sun | Pointless "Custom" layouts that change only the (wrong) Standard spacing | confirmed-test |
| U7 | PlanetStack stays selected after its partner leaves CarrierToCarrier | Unbuildable layout | confirmed-test |
| U8 | Schematic: stage 2 orbits at the carrier-1 angle; the Output badge sits on the wrong carrier; the fixed body is unmarked | Ravigneaux/Simpson animation contradicts the ratio | confirmed-read |
| U9 | Stale selection is rendered under a new topology (debounce window; stale rows clickable while searching) | Briefly nonsensical schematic | confirmed-read |
| U10 | Number inputs rewrite while typing (`0.5` hard to type, can't backspace past `150.`) | | confirmed-read |
| U11 | "Custom…" in the preset picker does nothing; rejected selections stay displayed | | confirmed-run |
| U12 | A sort click re-runs the whole search and resets Rows to 10; with a blank target, "best fit" means enumeration order | | confirmed-read |
| U13 | `index.html`: no viewport meta (phone layout), dark-only theming, no disabled/focus styles; disclosures and labels not accessible | | confirmed-read |
| U14 | Latent: `assignRole` throws for unreachable picker combinations; download URL revoked immediately (Firefox/Safari); `parseDouble("1,000.5")` fails | | confirmed-test / read (low) |

## Physics, geometry, export
| ID | Bug | Effect | Status |
|---|---|---|---|
| P1 | Exports (CSV/JSON/Pretty/OpenSCAD) print the synthetic sun for NSS stage 2; OpenSCAD draws a phantom sun gear | Wrong CAD model | **fixed** (was: confirmed-read) |
| P2 | OpenSCAD numbers use the default locale (`2,5000` under de/nl) | Broken `.scad` file on European systems | **fixed** (was: confirmed-read) |
| P3 | OpenSCAD gears are not phased (planet/ring offsets exist in `GearGeometry` but are unused); fixed bores are too large for small gears | Teeth interpenetrate in CAD | **fixed**, assumes the SCAD gear library puts a tooth at +X (unverified) |
| P4 | Pretty table separator misaligned; box characters garbled in the Windows console | Cosmetic | **fixed** (was: confirmed-read, confirmed-run) |
| P5 | `Kinematics.io` returns (S1, R2) whatever the topology | Wrong for Ravigneaux/Simpson (latent; tests only) | **fixed** (was: confirmed-test) |
| P6 | `Numeric.fixed` prints `0` for NaN/∞ | Silent 0 in CSV | **fixed** (was: confirmed-read (low)) |

## Design questions (need a decision)
| ID | Question | Facts |
|---|---|---|
| D1 | **CDCP semantics.** Decided: suns stay real gears by default; either sun can be made absent (ADR 0015). The reference drops the suns (sun min 2, no sun/planet factor, no spacing). Ours keeps the sun teeth range and the sun/planet factor | Configured like the reference, ours matches it exactly. With defaults we lose 10 % of designs in one fixture, more with wide bounds |
| D2 | **Ring outer diameter.** Decided: physical OD with a ring wall (ADR 0016). `m·(R+2)` (the reference's convention) is the size of an *external* gear; an internal ring's tooth roots reach `m·(R+2.5)`, before any wall | Module ~4 % too large for a given OD at R=60 |
| D3 | **Planet fit.** Decided: exact chord plus planet clearance (ADR 0016). Ours uses the exact chord `2·c2c·sin(π/N)`; the reference's `atan` approximation is stricter (an accidental clearance margin) | Ours accepts ~5–10 % more designs, some with near-zero tip clearance |
| D4 | **Efficiency model.** It counts meshes only and ignores power recirculation, which dominates high-ratio compound planetaries | Real efficiency at 50:1 can be far below the shown 94 % |
| D5 | **Single source of truth.** Decided and implemented (ADR 0015). Proposal: `Topology` replaces `Mode` everywhere; `Mode` becomes a CLI/URL alias for the four canonical layouts (ADR 0015) | Root cause of B2, B3, B5, U6, P5 |

## Reference program: what is wrong, right and unverified
The spec is physics (Willis kinematics, gear geometry), not the reference program
(`Compound Planetary Caluclator CDCP.py`, 2019). The reference is one independent
cross-check; where it disagrees with first principles, first principles win and
the differential test encodes the deviation explicitly.

| ID | Reference behaviour | Verdict | How we handle it |
|---|---|---|---|
| R1 | `MatchSets` loops over `Planet1`/`Planet2` without `global`, so `DecideIfPrint` always checks `PlanetSpacingOK[0]` | **Defect.** NSS spacing filter is meaningless: `nssfix_A_N3` gives 0 rows; `nss_A_N4` equals CDCP | `nssfix_*` fixtures repair it in memory; literal `nss_*` fixtures stay as `.fail` |
| R2 | `+NumPlanets` (line ~371) is a no-op; the loop keeps raising `MinModule` | **Defect.** Only `MinPlanets` is ever searched | Fixtures are generated per N, first pass only |
| R3 | Planet fit uses `atan(π/N)·c2c·2` and `6.28319` | **Defect** (approximation; stricter than the exact chord, 0.808 vs 0.866 at N=3) | We use the exact chord plus an explicit clearance (ADR 0016); comparison on the intersection of both predicates |
| R4 | `m1 = OD/(R1+2)` treats the ring as an external gear | **Defect.** Real ring exceeds the OD | ADR 0016: `m1 = (OD − 2·wall)/(R1+2.5)`; comparison applies the reference rule in the test |
| R5 | Exclusive `range()` maxima, strict ring/module bounds | Quirk | Translated in the test; ours are inclusive |
| R6 | Ratio `1/((Tr2 − Tr1·Tp2/Tp1)/Tr2 · Ts1/(Tr1+Ts1))` (CDCP: carrier factor 1) | **Correct** (re-derived from Willis; matches `RoleSolver` and `TabularSolver`) | Used as an oracle |
| R7 | Spacing `(S+R) % N == 0` per stage for Standard; none for sun-less stages | **Defect** in both the reference and our old code: with identical stepped planets a cross term `((Tp1·Tr2 − Tp2·Tr1)/gcd(Tp1,Tp2)) % N == 0` is also required (43–69 % of accepted stacked designs could not be assembled) | Fixed per ADR 0018; verified by an exact tooth-phase oracle (`assembly-conditions.md`). `phasedPinions` opts out |

## Not bugs
- Ratio sign is ignored by design when scoring.
