# ADR 0011 — Control-row vertical alignment and native rows-select

## Status
Accepted.

## Context

The results-table footer and the schematic-panel control row both shipped with hand-built compound controls that drifted out of vertical alignment with their flex-row neighbours and looked non-native:

- **Speed label** in the schematic header. Earlier attempts used `display: inline-flex; align-items: center; height: 22px` on the `<label>`. The 22-px fixed height constrains the label to the slider's *track* height (4 px) plus visible padding, not the slider thumb's apparent vertical centre. The parent `.controls` already does `align-items: center` over the natural row height (≈ slider thumb height ≈ 14 px), so giving the label its own 22-px box puts its centre below the row's apparent centre.
- **Rows-per-page chip** in the results-table footer. A wrapper `<label>` styled as a chip with `[number-input] rows` text inside — non-standard interaction (no native step buttons, focus ring on the wrapper rather than the input). Adjacent `Show more` / `Show all` buttons amounted to two ways of doing the same thing the chip already supported.

## Decision

### Speed-label / rpm-readout alignment

Drop the explicit `height: 22px` and the `inline-flex` wrapper from both labels. Use:

```css
.speed-label   { line-height: 1; align-self: center; … }
.rpm-readout   { line-height: 1; align-self: center; … }
```

`line-height: 1` collapses the line box to the text glyph height; `align-self: center` defers to the parent `.controls`'s `align-items: center`, which centres each child on the row's true midline (the slider thumb's vertical centre).

### Rows-select replaces chip + Show more + Show all

A native `<select>` with preset values `[10, 20, 50, 100, 200, All]` replaces the chip and both buttons. Picking "All" sets `displayCount` to the full result count, subsuming `Show all`. Picking a higher preset subsumes `Show more`. CSS uses `appearance: none` plus two `linear-gradient` triangles for the chevron — no SVG icon needed, and the dropdown options use the panel-1 background so they don't fight the dark theme.

## Consequences

- Far less custom DOM in the footer.
- The dropdown's interaction is the browser-native one (keyboard arrows, click-outside dismiss, etc.) — no custom event wiring needed.
- Show more / Show all are gone; users who relied on the +10 increment now pick the next preset (10 → 20).
- Alignment is robust against changes to the slider thumb size — the centring is now relative, not absolute.

## Verification

Visual: headless Edge screenshot at `docs/screenshots/round3-parts-A-B.png` shows the speed label and rpm readout on the schematic-row midline, and the native `Rows: [10 ▼]` dropdown in the results-table footer aligned with the status text on the left.
