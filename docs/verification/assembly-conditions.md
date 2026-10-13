# Planet assembly conditions

When can N planets sit at exactly 360°/N with every tooth mesh? This note derives the rule for
single stages, stacked (stepped-planet) compounds with and without suns, and compounds joined only by
central-shaft welds. It checks the rule against an independent brute-force oracle
(`core/src/test/scala/cpgc/verify/AssemblyOracle.scala`, `AssemblyConditionSpec.scala`).

## Result

| Case | Condition for N equally spaced planets |
|---|---|
| Single stage with sun | `(Ts + Tr) % N == 0` |
| Single stage without sun | none |
| Stacked, pinions identical across planets (any fixed half offset) | `(Ts_i + Tr_i) % N == 0` for each stage *i* that has a sun, **and** `((Tp1·Tr2 − Tp2·Tr1) / gcd(Tp1, Tp2)) % N == 0` |
| Stacked, each pinion phased individually | `(Ts_i + Tr_i) % N == 0` for each stage that has a sun |
| Non-stacked (central welds only) | `(Ts_i + Tr_i) % N_i == 0` for each stage that has a sun |

The cross term applies with both suns (Standard), with one (NSS), and with none (sun-less CDCP). With
the calculator's `R = S + 2P` (a virtual sun for a sun-less stage) it equals `(Tp1·Ts2 − Tp2·Ts1) / gcd`.
For equal pinions (`Tp1 = Tp2`) it reduces to the familiar split-ring rule `(Tr2 − Tr1) % N == 0`.
When both suns are present and both stage sums hold, it is equivalent to
`((Tp2·Ts1 − Tp1·Ts2) / gcd) % N == 0`.

**Before ADR 0018, production (ADR 0012 / 0015) was right for single stages and non-stacked compounds, and wrong for every
stacked compound with identical pinions. `Compounder.satisfiesSpacing` now enforces the cross term (unless `phasedPinions`).** It omitted the cross term, so it accepted designs that could not be
built. The smallest counterexample was `Tp1 = Tp2 = 6`, `Tr1 = 18`, `Tr2 = 19`, `N = 2` (rings differ by 1).
The NSS search winner from `SpacingExperimentSpec`, design B (`S1 53 / P1 11 / R1 75`,
`P2 14 / R2 100`, `N = 4`), does not assemble: `11·100 − 14·75 = 50`, and `50 % 4 = 2`. Design A (Standard) does assemble.

## Assumptions

- The planets sit at exactly `k/N` turns on one carrier for stacked compounds, or on each carrier otherwise.
- Phase only: a mesh is valid when, along the line of centres, a tooth of one gear sits centred in a gap of
  the other. Backlash, clearance and interference are ignored, and each mesh shares one module.
- Suns, rings and each planet body are free to rotate during assembly. Turning a gear by one tooth pitch is
  a symmetry of that gear.
- Stepped pinion: half 2 is rotated by `g` relative to half 1.
  (a) `g` is fixed and the same for every planet, as with a one-piece or 3D-printed stepped planet.
  (b) `g` is chosen freely at design time and is still the same for every planet.
  (c) Individually phased: planet *k* gets its own `g_k`.
  In (a) and (b) the rule does not depend on `g`, because a free central gear absorbs it. So (a) and (b) are the same condition.
- Welds in non-stacked compounds have an arbitrary fixed angular offset.

## Derivation

Angles are in turns. A gear with `T` teeth at rotation `α` has phase `u = frac(T·(θ − α))` in direction θ
(0 means a tooth is centred on θ). The two gears' tangents along the pitch line point in opposite directions
(external mesh) or the same direction (internal mesh), which gives the mesh conditions `u_sun + u_planet ≡ ½`
and `u_ring − u_planet ≡ ½` (mod 1). Planet *k* sits at `θ_k = k/N` with body angle `x_k`, and its half *i*
is at `x_k + g_i`. With σ for the sun angle and ρ for the ring angle:

    sun  i:  Ts_i(θ_k − σ_i) + Tp_i(θ_k + ½ − x_k − g_i) ≡ ½
    ring i:  Tr_i(θ_k − ρ_i) − Tp_i(θ_k − x_k − g_i)     ≡ ½

**Single stage.** Adding the two equations eliminates `x_k`: `(Ts+Tr)·θ_k − Ts·σ − Tr·ρ ≡ const`.
At `k = 0` this fixes σ and ρ. At `k = 1` it requires `(Ts+Tr)/N ∈ ℤ`. The condition is also sufficient,
because each `x_k` then follows from the sun equation. Without a sun, only the ring equation remains, and
`x_k` alone satisfies it, so there is no condition.

**Stacked.** Each half's ring equation is a congruence on the shared real `x_k`:
`Tp_i·x_k ≡ A_ik (mod 1)`. With `d = gcd(Tp1, Tp2)` and `p_i = Tp_i/d`, the two congruences have a
common solution iff `p2·A_1k − p1·A_2k ≡ 0 (mod 1)`. The reason: `x = (A_1 + j)/Tp1`, and `p2·j` runs
through every residue mod `p1`. Substituting gives

    (p1·Tr2 − p2·Tr1)·θ_k + (terms in ρ1, ρ2, g1, g2, constants) ≡ 0,

because the `Tp·θ_k` terms cancel (`p2·Tp1 = p1·Tp2`). At `k = 0` the free rings take up the constant,
including `g`. At `k = 1` the condition is `N | (Tp1·Tr2 − Tp2·Tr1)/d`. A present sun *i* adds its own
equation, which, given the ring equation, is exactly the single-stage sum `N | Ts_i + Tr_i`.
If the halves are phased individually (`g_k` per planet), the second congruence is satisfied by `g_k`, and
only the sums remain.

**Non-stacked.** Each stage's planets are separate gears, so each stage with a sun must assemble on its own:
`N_i | Ts_i + Tr_i` is necessary. It is also sufficient. Each stage leaves one condition on its central angles
at `k = 0`, and each condition has a nonzero coefficient on a body that the other stage does not constrain.
That makes the two conditions independent (rank 2), so any weld offsets can be accommodated.

**Why ADR 0012's NSS argument fails.** Stage 1 makes the planets congruent in the carrier frame only up to
their own rotations. `x_k` differs from planet to planet by multiples of `1/Tp1` plus a stage-1 term. Half 2
turns by the same angle, which is `Tp2/Tp1` of its own pitches, and that need not match ring 2's pattern at
`θ_k`. The residuals at planets 1..N−1 in `SpacingExperimentSpec` were dismissed as artefacts, but they can be
real (design B).

## Oracle

`AssemblyOracle` is a brute-force tooth-phase checker that works in exact rational arithmetic, with angles in
turns and phases in pitch units. It does not use any production spacing code.

- Planet 0's body angle is pinned. With the carrier held, the train keeps one degree of freedom that turns
  planet 0 and preserves every mesh. Tests confirm the verdict is the same for other pins.
- Every central gear is solved from its mesh with planet 0. The solution is unique up to one tooth pitch,
  which is a symmetry of that gear.
- For each planet `k ≥ 1`, the oracle tries all `Tp1` body angles per turn that satisfy its first mesh, and
  checks every other mesh with the exact predicate.
- Non-stacked compounds use a constructive search over 18 weld pairs with arbitrary offsets. Every witness is
  checked mesh by mesh. Necessity comes from the single-stage oracle.

A `false` from the stacked oracle is therefore exhaustive, not a sampling miss.

Results (diagnostic run, `-- *diagnostic*`; the default suite uses teeth 6..11 for Scala.js speed):

| Case (teeth 6..16, N 2..6, 73 205 cases each) | oracle OK | production OK | production false accepts | minimal |
|---|---|---|---|---|
| Standard (both suns) | 12 696 | 22 138 | 9 442 | 6/6/18 + 7/6/19, N=2 |
| NSS (S2 absent) | 15 146 | 34 606 | 19 460 | 6/6/18 + –/6/19, N=2 |
| S1 absent (mirror of NSS) | 15 146 | 34 606 | 19 460 | –/6/18 + 7/6/19, N=2 |
| Sun-less CDCP | 22 371 | 73 205 | 50 834 | –/6/18 + –/6/19, N=2 |

The oracle matched the verified rule in every case. It matched for `g = 0` and `g = 5/37` turn, and
over five offsets on teeth 6..9. Individually phased pinions matched the per-stage (production) rule
exactly. Production never rejects an assemblable design. Single stage: all 3 125 cases (teeth 6..30,
N 2..6) matched `(Ts+Tr) % N == 0`, which is `GearSet.evenSpacing`. Non-stacked: 300 000 cases
(teeth 6..10, N 2..5, 18 weld pairs, with and without suns) showed 0 disagreements with the per-stage rule.
`.fail` tests pin the production gap for Standard, NSS and sun-less CDCP.

## Recommendation

1. **Enforce the cross term for every stacked (`PlanetStack`) topology**, whatever its suns:
   `((Tp1·Tr2 − Tp2·Tr1) / gcd(Tp1, Tp2)) % N == 0`, plus `(S + R) % N == 0` for each stage that has a
   sun. Keep the per-stage rule for single-stage and non-stacked topologies; it is correct there. The cross
   term needs no sun, so `R = S + 2P` with a virtual sun works unchanged.
2. **Default to identical pinions (a/b).** One STL is printed N times, so there is no per-planet bookkeeping
   and no wrong-slot assembly. This is the standard engineering convention, and the half offset `g` is free:
   CAD can align the halves however is convenient, because the suns and rings take up `g`. The OpenSCAD
   export must then phase the suns and rings (see bug P3).
3. **Individually phased pinions as an opt-in only.** 3D printing makes N different pinions cheap, and they
   recover every design that production accepts today (43 % of Standard, 56 % of NSS and 69 % of sun-less
   designs above are buildable *only* this way). Offer it only once exports emit each planet's half offset
   `g_k` and label which slot it goes in. Otherwise the user gets a design they cannot assemble.
4. **Supersede the spacing table in ADR 0012 / ADR 0015 §5** with a new ADR. The "Open" question in
   ADR 0015 is resolved: a joint condition exists for any stacked pinion, not only the sun-less one.
   Correct the `SpacingExperimentSpec` header and the `Coupling.assemblable` doc, and pin the change in the
   bug register.
5. **Planet clearance.** Clearance does not interact with phase: it depends only on centre distance, N and
   tip diameters. Check it per stacked half, at that half's own axial level. Unequal spacing could rescue
   designs that fail on phase, but it changes the clearance check and the balance. Keep equal spacing as the
   model.

Confidence: high. The derivation is short and exact, and an exhaustive exact oracle independently confirms
it on two platforms. The assumptions above, especially the phase-only mesh model, are the limits of the claim.
