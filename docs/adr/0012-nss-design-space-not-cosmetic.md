# ADR 0012 — NSS is a design-space expansion via dropping the stage-2 spacing requirement

## Status
Accepted. Supersedes the original 0012 (whose math was wrong and whose framing of two competing spacing conditions was incorrect).

## Context

Removing the second sun gear from a Standard compound is a real engineering choice with real implications for the calculator's search:

- Algorithmically, it widens which compound designs the search will return.
- Physically, a built Standard compound continues to function when the second sun is unscrewed (the sun was a passive gear meshing along; removing it leaves planets and ring untouched). The same gear-count tuple is therefore both Standard-valid (with the sun) and NSS-valid (without).

Earlier versions of this ADR claimed NSS imposed a separate `R2 % N == 0` spacing condition and rewrote the module-matching formula in terms of `R2 − P2`. Both claims were wrong. There is one spacing condition in play — single-stage Willis `(S + R) % N == 0` — and NSS *drops* it for stage 2 rather than replacing it with something else.

## Decision

The compound search uses a single enumerator pool of `(S, P, R = S + 2·P)` candidates filtered by tooth-count range, sun-planet factor, and the planets-don't-overlap geometric chord check. Spacing rules are applied per mode at pair-up time in `Compounder.compound`:

| Mode          | Stage 1 spacing             | Stage 2 spacing             |
| :------------ | :--------------------------- | :--------------------------- |
| `SingleStage` | `(S + R) % N == 0` required | —                            |
| `Standard`    | `(S + R) % N == 0` required | `(S + R) % N == 0` required |
| `NSS`         | `(S + R) % N == 0` required | **dropped**                  |
| `CDCP`        | not required                | not required                 |

Module matching is `m2 = m1 · (S1 + P1) / (S2 + P2)` in every mode (the `(R − P)/2` form mentioned in earlier drafts was just `(S + P)/2` rewritten using `R = S + 2P`, the same number, not a different formula).

## Derivation — why NSS drops stage 2's spacing

Single-stage Willis `(S + R) % N == 0` is the assembly condition for a sun-planet-ring stage with N symmetric planet positions. It comes from requiring the planet's tooth orientation to satisfy *both* the sun-planet (external) mesh and the planet-ring (internal) mesh phase invariants at every planet position around the carrier. The two mesh invariants impose a joint requirement on the planet's tooth phase at each carrier angle, and `(S + R) % N == 0` is the integer condition that lets all N planets be carrier-symmetric clones meshing with both gears at once.

In a compound NSS stage 2, the planet body is rigidly coupled to the stage-1 planet via a shared shaft. Stage 1's Willis-symmetric stage (which the NSS mode still requires of stage 1) places all stage-1 planets at the same tooth orientation in the carrier frame. The coupled stage-2 planets therefore also share an orientation. With only the planet-ring mesh remaining in stage 2 (no sun-planet mesh), the single mesh phase invariant — applied at every planet position — is automatically satisfied because the planets are clones meshing with a uniform ring tooth pattern at angular positions inherited from stage 1.

So NSS doesn't impose a new condition; it removes the second mesh's contribution to the joint phase requirement.

## Consequences

- NSS design space ⊇ Standard design space, for any given gear-count ranges. Any pair Standard accepts is also accepted by NSS; some pairs accepted by NSS are rejected by Standard (those where stage-2's `(S + R) % N != 0`).
- Targeted searches in NSS find at least as good a best fit as Standard does for any target ratio, since Standard's design space is a subset.
- The previous `Enumerator.gearSetsSunlessStage2`, the virtual-`S2 = R2 − 2·P2` storage trick, and the `R2 % N == 0` filter were all unnecessary inventions; they're removed.

## Test invariants

`core/src/test/scala/cpgc/NssSearchSpec.scala`:

1. NSS designs are a superset of Standard designs (cross-product search comparison).
2. NSS strictly expands the design space — at least one pair is NSS-valid and Standard-invalid for the default config.
3. Module matching uses the same `(S2+P2)` denominator in both modes.
4. Stage 1 spacing is required for NSS; a Willis-failing stage 1 causes the design to be rejected.
5. NSS accepts a stage 2 that fails Standard's stage-2 spacing as long as stage 1 is spacing-OK.
6. NSS ratio equals Standard ratio for triples that satisfy both modes (Ts2 invariance of the ratio formula).
7. NSS efficiency strictly exceeds Standard efficiency for the same triple (one fewer mesh).
8. NSS targeted inverse solver returns designs near the target ratio.

The earlier "design-space disjointness" framing has been retired — the two sets aren't disjoint; one contains the other.

## Related notes

- The earlier `SpacingExperimentSpec` numbers that appeared to show compound mesh-phase residuals failing for Standard designs were artefacts of misusing `GearGeometry.planetPhaseOffset` (a visual per-planet rotation offset, wrapped to the shortest arc) as if it were a planet's physical body rotation. That spec is retained as diagnostic-only with a header comment explaining the misinterpretation.
