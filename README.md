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

- **Search**: the page drives the same `SearchEngine` as the CLI in ~12 ms chunks, capped at 5 s
  (then "best so far, truncated, narrow your ranges"). Changing the sort re-ranks in place when the
  run finished untruncated and kept every design; otherwise it searches again. Without a target the
  default sort is the lowest ratio, and the pill says so.
- **Topology**: pick a preset (including "CDCP (sun-less)"), or open "Advanced topology" for per-stage
  sun toggles (a sun that is pinned or welded cannot be removed; the toggle says why), couplings,
  Input/Output/Fixed bodies and "Individually phased pinions" (stacked layouts only: each planet gets
  its own twist, so more designs assemble but the planets are different parts). "Advanced gear
  constraints" has min module, max planet/sun ratio, ring wall (mm) and planet clearance (modules).
- **Targets**: separate several with `;` or a space; a comma between digits is a decimal comma
  (`53,12` = 53.12, `100, 53.12` = two targets). Invalid input shows an inline error, never a hang.
- **URL**: the address bar mirrors the whole state and is read back on load. Parameters: `mode`
  (`standard|single|cdcp|nss|cdcp-sunless`), `coupling1`, `coupling2` (`Coupling` names), `input`,
  `output`, `fixed` (all three, `Body` names), `absent` (e.g. `S1,S2`), `sun`, `planet`, `ring`,
  `planets` (`lo-hi` or a single value), `od`, `minmod`, `ringwall`, `clearance`, `spfact`, `phased=1`,
  `target`, `sort` (`bestfit|ratio|efficiency|simplicity`). Values are percent-encoded.
- Light and dark themes follow `prefers-color-scheme`; the layout is a single column on phones.

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
