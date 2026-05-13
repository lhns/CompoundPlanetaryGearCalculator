package cpgc.domain

/** Gear module in mm/tooth. Pitch diameter = module * teeth. */
opaque type Module = Double

object Module:
  def apply(m: Double): Module =
    require(m > 0.0 && m.isFinite, s"Module must be positive and finite, got $m")
    m

  def unsafe(m: Double): Module = m

  extension (m: Module)
    inline def value: Double  = m
    inline def toDouble: Double = m
    inline def *(k: Double): Module = (m: Double) * k

  given Ordering[Module] = Ordering.Double.TotalOrdering
