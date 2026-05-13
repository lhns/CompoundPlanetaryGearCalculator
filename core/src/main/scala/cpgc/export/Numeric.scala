package cpgc.`export`

/** Locale-free fixed-point formatter. `String.format(Locale.ROOT, "%.Nf", d)`
  * works on JVM but pulls in `java.util.Locale` / `java.text.DecimalFormatSymbols`
  * that Scala.js's stdlib doesn't fully link. This rolls a tiny equivalent so
  * Csv / Json / OpenScad all cross-compile to JS. */
object Numeric:

  def fixed(d: Double, decimals: Int): String =
    if d.isNaN || d.isInfinite then "0"
    else
      val factor  = math.pow(10.0, decimals.toDouble).toLong
      val rounded = math.round(d * factor.toDouble)
      val sign    = if rounded < 0 then "-" else ""
      val abs     = math.abs(rounded)
      val intPart = abs / factor
      val fracPad = abs % factor
      val fracStr = padLeftZeros(fracPad.toString, decimals)
      if decimals == 0 then s"$sign$intPart"
      else s"$sign$intPart.$fracStr"

  private def padLeftZeros(s: String, width: Int): String =
    if s.length >= width then s
    else "0" * (width - s.length) + s
