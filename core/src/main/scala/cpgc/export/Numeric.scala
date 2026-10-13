package cpgc.`export`

import scala.math.BigDecimal.RoundingMode

/** Locale-free fixed-point formatter. `String.format` honours the default
  * locale (decimal comma under de/nl) and `java.util.Locale` does not fully link
  * on Scala.js, so every export (Csv, Json, OpenScad, Pretty) goes through here. */
object Numeric:

  /** Beyond this magnitude `d * 10^decimals` no longer fits a Long exactly. */
  private val LongSafe = 9.0e15

  /** `d` with exactly `decimals` digits (0..15) after the point, rounded half up.
    * Non-finite values print as `NaN`, `Inf` and `-Inf`, never as a number. */
  def fixed(d: Double, decimals: Int): String =
    require(decimals >= 0 && decimals <= 15, s"decimals must be in 0..15, got $decimals")
    if d.isNaN then "NaN"
    else if d == Double.PositiveInfinity then "Inf"
    else if d == Double.NegativeInfinity then "-Inf"
    else
      val factor = math.pow(10.0, decimals.toDouble)
      if math.abs(d * factor) >= LongSafe then
        BigDecimal(d).setScale(decimals, RoundingMode.HALF_UP).bigDecimal.toPlainString
      else
        val rounded = math.round(d * factor)
        val sign    = if rounded < 0 then "-" else ""
        val abs     = math.abs(rounded)
        val f       = factor.toLong
        val intPart = abs / f
        if decimals == 0 then s"$sign$intPart"
        else s"$sign$intPart.${padLeftZeros((abs % f).toString, decimals)}"

  /** JSON number: `null` when not finite. */
  def jsonNumber(d: Double, decimals: Int): String =
    if d.isNaN || d.isInfinite then "null" else fixed(d, decimals)

  private def padLeftZeros(s: String, width: Int): String =
    if s.length >= width then s
    else "0" * (width - s.length) + s
