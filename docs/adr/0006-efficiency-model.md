# ADR 0006 — Efficiency model

## Status
Accepted.

## Context
Users care about whether a high-ratio gearbox will actually transmit power usefully. A 1000:1 ratio with 30% efficiency is rarely useful.

## Decision
We expose a coarse efficiency estimate per design:
- Per-mesh efficiency constant: **0.985** (mid-range textbook value for well-cut, lubricated involute spur gears).
- A standard sun-driven planetary stage has two meshes in the power path (sun↔planet, planet↔ring): `η_stage ≈ 0.985² ≈ 0.970`.
- A compound has two stages: `η_total ≈ η_stage₁ · η_stage₂`.
- NSS (no second sun) is modelled with one fewer mesh in stage 2.

## Limitations
- Real efficiency depends on tooth quality, surface finish, lubrication, load, speed, profile shift, backlash. Our number is for ranking only.
- We do not model carrier bearings, seals, or churning losses.

## Consequences
- The number is good enough to filter out obviously bad designs (e.g. very high tooth-mesh counts) and to give users a first-order intuition. It is **not** a substitute for testing.
