package cpgc.domain

/** Topology of the planetary gearbox under search.
  *
  *  - Standard:    two sun-driven stages share a carrier.
  *  - CDCP:        carrier-driven compound; no sun-input constraint.
  *  - NSS:         second stage has no sun gear.
  *  - SingleStage: one planetary stage only.
  */
enum Mode:
  case Standard, CDCP, NSS, SingleStage

object Mode:
  def parse(s: String): Either[String, Mode] =
    s.trim.toLowerCase match
      case "standard" | "std"           => Right(Standard)
      case "cdcp"                        => Right(CDCP)
      case "nss"                         => Right(NSS)
      case "single" | "singlestage" | "single-stage" => Right(SingleStage)
      case other                         => Left(s"Unknown mode: $other")
