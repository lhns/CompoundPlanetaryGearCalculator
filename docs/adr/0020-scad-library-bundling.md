# ADR 0020 — Bundling gears.scad at download time

## Status
Accepted. Complements ADR 0007 (which stays as is).

## Context
The OpenSCAD export calls `spur_gear` / `ring_gear` from Dr Jörg Janssen's `gears.scad` (English version: github.com/chrisspen/gears). Users had to find and download it themselves. Its licence is CC BY-NC-SA, so we cannot vendor it in the repo or the Docker image.

## Decision
- The web page fetches the library from a pinned commit (`OpenScad.LibraryUrl`, raw.githubusercontent.com, CORS-enabled) when the user downloads, and offers three outputs: a single `.scad` with the library inlined (`OpenScad.bundle`, between marker comments, library header untouched), a zip with the design and `gears.scad` (store-only zip written in `web`), or the plain `.scad`. If the fetch fails the plain file is saved and a warning toast says so.
- Nothing is redistributed by us: the user's browser downloads the file from its author's repository. The emitted header names the library, its licence and home.
- `rim_width` is derived so the library's ring outer diameter (`2·(ra + rim_width)`) equals `Stage.ringOuterDiameter` (ADR 0016); the previous value, the bare wall, made the ring about m/6 too small in diameter.
- Phasing assumptions (tooth on +X for `spur_gear`, gap on +X for `ring_gear`) were checked against the pinned source.

## Consequences
- Bundled and zip downloads need network access to GitHub. The CLI still emits the plain file.
- The pinned commit is bumped by hand; a bump must re-check the phasing and the `ring_gear` OD formula.
- Inlining equals `include`: it relies on the library having no top-level geometry and no top-level names that clash with the emitted file (`$fn`, `pi`, `rad`, `clearance` only).
