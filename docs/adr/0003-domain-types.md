# ADR 0003 — Opaque domain types

## Status
Accepted.

## Context
Tooth counts, modules, and gear ratios are all numbers, but mixing them up is a real bug class (e.g. passing a teeth count where a module is expected). We want compile-time distinction without runtime overhead.

## Decision
- `Teeth`, `Module`, `Ratio` are Scala 3 `opaque type` aliases for `Int` / `Double`.
- Smart constructors validate at boundaries (`Teeth(0)` throws; internal hot paths use `Teeth.unsafe`).
- Arithmetic stays close to the primitive: extension methods are `inline` so the JIT/Scala.js linker sees plain `Int`/`Double` operations.

## Alternatives considered
- **Value classes (`AnyVal`)**: pre-Scala 3 idiom; boxes in many situations. Opaque types are strictly better.
- **Plain `Int` / `Double`**: smaller blast radius but loses type safety. Rejected.
- **`refined` / `iron`**: more guarantees, more dependencies and compile cost. Overkill for this domain.

## Consequences
- Zero runtime overhead in tight enumeration loops.
- `GearSet(Teeth(20), Module(...))` doesn't compile — protects against argument swaps.
- Slight ceremony when extracting `.value` for arithmetic; we provide `inline def *` etc. where it pays off.
