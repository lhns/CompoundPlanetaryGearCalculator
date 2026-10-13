# ADR 0018 — Assembly condition for stacked (stepped) planets

## Status
Accepted. Supersedes the spacing table of ADR 0012 and §5 of ADR 0015 for stacked topologies.

## Context
Production required `(Ts + Tr) % N == 0` on each stage that has a sun, and nothing else, and so did the
reference program. An exact tooth-phase oracle (`docs/verification/assembly-conditions.md`,
`AssemblyConditionSpec`) shows this is right for single stages and for compounds joined only by central
welds. It is wrong for stacked planets when every planet is the same part (one STL printed N times): over
small tooth ranges it accepts 43 % (Standard), 56 % (NSS) and 69 % (sun-less CDCP) of designs that cannot be
assembled. The smallest counterexample is `Tp1 = Tp2 = 6, Tr1 = 18, Tr2 = 19, N = 2`.

## Decision
- Stacked topologies with identical pinions (the default) require, in addition to the sum rule for each
  stage with a sun:
  `((Tp1·Tr2 − Tp2·Tr1) / gcd(Tp1, Tp2)) % N == 0`
  This holds with or without suns; virtual suns are irrelevant because it uses ring teeth.
- `SearchConfig.phasedPinions: Boolean = false`. When true, each planet's two halves are cut with a
  planet-specific twist, the cross term is dropped, and exports must give each planet's twist and slot.
- Single stages and non-stacked compounds are unchanged.

## Consequences
- Fewer stacked designs, all buildable from identical planets. Opting into `phasedPinions` restores the
  rest at the cost of N different planet parts.
- `SpacingExperimentSpec` and ADR 0012's claim that the residuals were "artefacts" were wrong. Those
  residuals were this condition.
