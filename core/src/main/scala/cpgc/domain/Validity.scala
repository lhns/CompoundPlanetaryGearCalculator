package cpgc.domain

/** Per-design validity / warning signal. The UI surfaces non-`Ok` values as a
  * warning glyph on the design row and as a notice on the schematic detail
  * line. `Compounder` produces `Warning`s for a decoupled stage, a trivial
  * passthrough (ratio 1) and a near-singular solve.
  */
enum Validity:
  case Ok
  case Warning(reason: String)
