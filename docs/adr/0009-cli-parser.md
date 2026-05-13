# ADR 0009 — CLI parser: decline

## Status
Accepted.

## Context
The CLI needs typed argument parsing, automatic `--help`, and good error messages for invalid combinations.

## Decision
Use **decline** (`com.monovore.decline`). Scala 3 friendly, Validated-based composition, generates help text automatically.

## Alternatives considered
- **scopt**: works fine but is mutable-builder-based and the help output is less polished.
- **case-app**: heavier macro use; we don't need its features.
- **picocli**: Java-first; awkward in Scala 3.

## Conventions
- Top-level command: `cpgc`. Single subcommand for now (`search`); structured as a `CommandApp` so additional subcommands can be added (`emit-scad`, `validate`, …) without rework.
- Flag naming: kebab-case, prefixed by domain where helpful (`--min-sun`, `--max-planet`, `--min-module`).
- Repeatable options (`--target`) accept multiple values.

## Consequences
- Adding new subcommands is local — no parser-wide refactors.
- All option validation is `Validated`-based and accumulates errors into one report.
