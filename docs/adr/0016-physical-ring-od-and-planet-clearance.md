# ADR 0016 — Physical ring outer diameter and planet tip clearance

## Status
Accepted. Supersedes the module derivation used since ADR 0004 and refines ADR 0005.

## Context
The module was derived as `m1 = OD / (Tr1 + 2)`, the tip circle of an *external* gear, and the reference
program uses the same convention. An internal ring's tooth roots sit at pitch + 1.25·m, so the real part is
larger than the stated OD and its wall is negative. For example, at R = 60 the module comes out about 4 %
too large. Separately, the planet-overlap check (ADR 0005) accepts zero tip-to-tip clearance between
adjacent planets.

## Decision
- New `SearchConfig` parameters: `ringWall` (mm, default 2.0) and `planetClearance` (in modules,
  default 0.5).
- The OD bounds the ring's outside: `OD = m·(Tr + 2.5) + 2·ringWall`, so `m1 = (OD − 2·ringWall) / (Tr1 + 2.5)`.
  The search gives no result when that is ≤ 0. Stage 2's module is matched as before for stacked planets and
  is otherwise `m2 = m1`. Each stage reports its own ring OD with the same formula.
- Planets fit iff `Tp + 2 + planetClearance < (Ts + Tp)·sin(π/N)` (tooth units; the module cancels).
- The CLI gets `--ring-wall` and `--planet-clearance`. The web UI gets both under "Advanced gear constraints".

## Consequences
- Results differ from the reference program. The differential test compares design sets and ratios with
  module filtering applied by the reference's own convention, so it keeps verifying the ratio math.
- Clearance and wall are explicit, documented knobs instead of accidents of approximation.
