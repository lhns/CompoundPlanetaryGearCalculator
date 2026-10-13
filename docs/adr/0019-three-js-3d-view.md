# ADR 0019 — three.js 3D view

## Status
Accepted.

## Context
The schematic is 2D. A stacked planet, a shaft welding Ring 1 to Sun 2 or the two stages' axial order are easier to read in 3D. The web build is Scala.js with `ModuleKind.NoModule`, and the page must work opened from disk (`file://`) and offline, so it cannot load ES modules or a CDN.

## Decision
- Vendor three.js **r158 `build/three.min.js`** (MIT, licence alongside) in `web/src/main/resources/vendor/three/`, loaded by `index.html` as a classic script before `main.js`. `webDist` / `webDistDev` copy `vendor/`. r158 is the last release that ships the global build; it logs a deprecation warning, which is accepted.
- Hand-written minimal `@JSGlobal("THREE.…")` facades (`web/three/Three.scala`), only for what is used. No npm, no bundler.
- Own orbit controls in Scala (`OrbitState` pure maths, `OrbitControls` DOM events); no `OrbitControls` dependency.
- The scene is a pure `Layout3D` (clusters of welded bodies, planets, connectors) built from the same tooth outlines, `Kinematics.at` angles and `Phasing` as the schematic; `ThreeScene` turns it into meshes. The renderer exists only while the 3D view is mounted. `view=3d` is in the URL.
- Colours follow the schematic: pinned roles, then coupling 1 / 2 (amber / teal), then stage tints.

## Consequences
- Works from `file://` and offline; a 650 KB vendored file in the repo.
- Upgrading past r160 means an ES module build (`import` of `three.module.js`), which browsers refuse from `file://`; that would need a bundler or a served page, and a new ADR.
- Coupling colours are duplicated from `Schematic` (hex pairs in `ThreeView`); keep them in step.
- Without WebGL the view shows a message and the schematic still works.
