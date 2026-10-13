# Compound Planetary Gear Calculator

A Scala 3 toolkit for designing compound planetary gearboxes. Enumerates tooth
combinations under geometric and physical constraints, computes overall ratios,
ranks them against user-supplied targets, and exports CSV / JSON / OpenSCAD.

Three faces, one shared algorithm:

- `cli` — command-line search with a pretty table.
- `web` — Scala.js browser UI with a live SVG schematic.
- `core` — library; cross-built for JVM and JS.

## Layout

```
build.sbt
core/   — pure algorithm, cross-built JVM+JS, zero deps
cli/    — JVM CLI (decline)
web/    — Scala.js + Laminar UI
docs/adr/ — architecture decision records
```

## Quickstart

### CLI

```
sbt "cli/run --target 53.12 --max-sun 80 --max-planet 20 --top 10"
```

Useful flags:

- `--mode standard|cdcp|nss|single`; `--sunless 1|2|both` removes a stage's sun gear (`--mode cdcp --sunless both` is a carrier-driven compound with no sun gears)
- `--min-sun / --max-sun`, `--min-planet / --max-planet`, `--min-ring / --max-ring`
- `--min-planets / --max-planets`
- `--target <ratio>` (repeatable)
- `--od <mm>`  first-stage ring outer diameter, `--ring-wall <mm>`, `--planet-clearance <modules>`
- `--sort best|ratio|efficiency|simplicity` ranking (default: closest to the targets, or smallest ratio without targets)
- `--phased-pinions`  stacked planets cut with a planet-specific twist (drops the identical-pinion assembly rule, ADR 0018)
- `--min-module <mm/tooth>`
- `--csv out.csv` / `--json out.json` / `--scad design.scad`

### Web UI

```
sbt web/fastLinkJS
# then serve web/src/main/resources alongside web/target/scala-*/cpgc-web-fastopt/
```

A minimal `index.html` is included; point a static file server at the `web` output
directory or copy `main.js` next to `index.html` and open it in a browser.

### Library

```scala
import cpgc.Calculator
import cpgc.constraints.{SearchConfig, TeethBounds}
import cpgc.domain.{Mode, Ratio}

val cfg = SearchConfig(
  mode    = Mode.Standard,
  targets = Seq(Ratio(53.12))
)
val designs = Calculator.best(cfg)
```

## Testing

```
sbt test
```

Property tests (`scalacheck`) cover the tooth invariants and spacing checks.
Unit tests lock the compound-ratio math and CSV determinism.

## Architecture

See `docs/adr/` for the design decisions:

1. Language and build — Scala 3 + sbt + cross JVM/JS.
2. Pure functional core — immutable model, `LazyList` enumeration.
3. Domain types — opaque types for `Teeth` / `Module` / `Ratio`.
4. Search strategy — enumerate-then-pair with bounded top-K.
5. Planet overlap formula — chord-based geometry.
6. Efficiency model — coarse per-mesh estimate.
7. CAD export scope — OpenSCAD only.
8. Web architecture — Laminar + SVG schematic.
9. CLI parser — decline.
