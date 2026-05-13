# ADR 0007 — CAD export scope

## Status
Accepted.

## Context
Users want to take a winning design directly into a printable / machinable model.

## Decision
- **In scope:** OpenSCAD emission. We generate a parametric `.scad` that places sun, N planets (rotated around the carrier), and a ring gear at the computed modules and center distances. We assume the user supplies a gear library (`use <gears.scad>`).
- **Out of scope:** STEP / IGES / native solid-body export. These require a B-rep kernel (OpenCascade) we are not pulling into a pure Scala 3 cross-build.

## Rationale
OpenSCAD is free, scriptable, and ubiquitous in the 3D-printed-gearbox community. Anyone needing STEP can export it from OpenSCAD via mesh tools or migrate to a CAD package using our printed parameters.

## Consequences
- Zero binary dependencies in `core`.
- The `.scad` is human-editable: users can tweak fillets, helical angles, bearing pockets etc. on top of our skeleton.
