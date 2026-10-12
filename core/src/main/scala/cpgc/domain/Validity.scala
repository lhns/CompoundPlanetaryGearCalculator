package cpgc.domain

/** Per-design validity / warning signal. UI surfaces non-`Ok` values as a
  * warning glyph on the design row and as a notice on the schematic detail
  * line; nothing currently produces a `Warning(...)`. The infrastructure is
  * in place for future conditions (e.g. flagging designs that pass our
  * filters but fail a downstream tool's stricter check, or for cases where
  * we want to surface a "try-fit before manufacturing" hint without
  * filtering the design out of results).
  */
enum Validity:
  case Ok
  case Warning(reason: String)
