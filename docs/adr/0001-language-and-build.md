# ADR 0001 — Scala 3 + sbt + cross JVM/JS

## Status
Accepted.

## Context
The calculator must be usable both as a fast command-line tool and as a browser-hosted interactive UI. The algorithm is small, pure, and arithmetic-heavy. We want a single source of truth for the search core so the CLI and the web cannot drift.

## Decision
- **Language:** Scala 3.5.x. We get opaque types (zero-overhead nominal wrappers around `Int`/`Double`), `enum`s for closed sums, and significant-indentation syntax — all useful for a small domain layer that we want to keep dense and accurate.
- **Build:** sbt with `sbt-crossproject` and `sbt-scalajs`. Cross-compile the `core` module to JVM and JS. Separate `cli` (JVM-only, depends on `core.jvm`) and `web` (JS-only, depends on `core.js`).
- **No effect system.** The core is synchronous and pure; we use `LazyList` for lazy enumeration. Effects (file I/O, stdout) live only in the CLI module.

## Consequences
- One algorithmic source, two deploy targets.
- Slightly higher build complexity than a single-platform project, but `crossProject(... ).crossType(CrossType.Pure)` keeps source layout straightforward (`core/src/main/scala`).
- sbt is the most boring/stable choice for cross-compiling. Mill and scala-cli are appealing but have less mature Scala.js wiring for multi-module projects.
