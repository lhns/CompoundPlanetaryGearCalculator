package cpgc.domain

opaque type Teeth = Int

object Teeth:
  def apply(n: Int): Teeth =
    require(n > 0, s"Teeth count must be positive, got $n")
    n

  def unsafe(n: Int): Teeth = n

  extension (t: Teeth)
    inline def value: Int       = t
    inline def toInt: Int       = t
    inline def toDouble: Double = t.toDouble
    inline def +(o: Teeth): Teeth = (t: Int) + (o: Int)
    inline def *(k: Int): Teeth   = (t: Int) * k

  given Ordering[Teeth] = Ordering.Int
