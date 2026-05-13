# ADR 0008 — Web architecture

## Status
Accepted.

## Context
The web UI needs to (a) bind a form to a `SearchConfig`, (b) run the same `Calculator.search` as the CLI, and (c) draw a live schematic of the highlighted design.

## Decision
- **Laminar** for reactive UI. `Var[T]` for form state, `Signal[T]` for derived views. Tiny runtime; idiomatic Scala 3.
- **scala-js-dom** for raw DOM where needed.
- **SVG, not Canvas** for the schematic. SVG is declarative — we map a `CompoundDesign` to a `Vector[SvgElement]` and Laminar reconciles. Vector output also looks good at any zoom.
- **No Web Worker** for now: synchronous search on the main thread, capped at `topK` results. For very large sweeps we will revisit with a Worker hosting the same `core.js` artifact.

## Consequences
- The same algorithm code runs in the browser as in the CLI — zero divergence.
- UI changes are typed end-to-end; misnaming a field is a compile error.
- A large unbounded search could jank the main thread. The form's `top K` and tooth-bound caps make this unlikely in practice; the Worker upgrade is small if needed.
