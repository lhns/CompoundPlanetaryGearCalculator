package cpgc.domain

/** Overall gearbox reduction ratio. Negative values indicate direction reversal. */
opaque type Ratio = Double

object Ratio:
  def apply(r: Double): Ratio =
    require(r.isFinite, s"Ratio must be finite, got $r")
    r

  def unsafe(r: Double): Ratio = r

  extension (r: Ratio)
    inline def value: Double    = r
    inline def toDouble: Double = r
    inline def abs: Ratio       = math.abs(r: Double)

  given Ordering[Ratio] = Ordering.Double.TotalOrdering
