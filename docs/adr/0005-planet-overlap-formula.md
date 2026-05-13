# ADR 0005 — Planet overlap formula

## Status
Accepted.

## Context
Adjacent planet gears must not collide with each other as they orbit the sun. We check this geometrically before admitting a candidate stage.

## Decision
Use the chord between adjacent planet centers:

```
C2C   = module * (Ts + Tp) / 2     // center-to-center between sun and planet
chord = 2 * C2C * sin(π / N)        // straight-line distance between adjacent planet centers
```

Planets fit without overlap iff each planet's outer diameter is strictly less than the chord:

```
planetOD = module * (Tp + 2)        // addendum = 1*module on each side
planetOD < chord
```

Module is a common factor on both sides, so the check reduces to a tooth-count inequality:

```
(Tp + 2) < (Ts + Tp) * sin(π / N)
```

## Derivation
N planet centers lie on a circle of radius `C2C` centered on the sun axis, equispaced by `2π/N` radians. The straight-line distance between adjacent centers is `2 * C2C * sin(π/N)` (isoceles-triangle chord formula). For non-overlap, the sum of planet radii — which equals one planet's diameter for equal planets — must fit inside that chord.

## Consequences
- We have a single, defensible non-overlap test independent of module.
- We do *not* use `atan(angle) * C2C` for the chord; that approximation underestimates the chord for large N and would over-reject borderline designs.
