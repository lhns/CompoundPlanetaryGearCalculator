package cpgc.web

/** The one colour table of the web UI, shared by the SVG schematic and the 3D view.
  *
  * A `Tone` says what a part is (pinned role, coupling 1 / 2, stage tint); `GearboxScene` assigns it
  * with the precedence role > coupling > stage tint. Role and coupling colours have a dark and a light
  * theme value; stage tints are the same in both. */
object Palette:

  enum Tone:
    case Input, Output, Fixed, Cpl1, Cpl2, Carrier, Sun1, Ring1, Planet1, Sun2, Ring2, Planet2

  /** (dark theme, light theme) as 0xRRGGBB. */
  private val Themed: Map[Tone, (Int, Int)] = Map(
    Tone.Input  -> (0x7ee787 -> 0x1a7f37),
    Tone.Output -> (0x58a6ff -> 0x0969da),
    Tone.Fixed  -> (0xff7b72 -> 0xcf222e),
    Tone.Cpl1   -> (0xffb000 -> 0xb35c00),
    Tone.Cpl2   -> (0x19c3b1 -> 0x007c7c)
  )

  private val Tints: Map[Tone, Int] = Map(
    Tone.Sun1 -> 0xffd166, Tone.Ring1 -> 0x118ab2, Tone.Planet1 -> 0x06d6a0,
    Tone.Sun2 -> 0xff9f1c, Tone.Ring2 -> 0x9d4edd, Tone.Planet2 -> 0x73d2de,
    Tone.Carrier -> 0x8b949e
  )

  def hex(t: Tone, light: Boolean): Int =
    Themed.get(t).fold(Tints(t))((d, l) => if light then l else d)

  def hexString(c: Int): String = f"#$c%06x"

  /** CSS custom property of a themed tone, defined by `styleBlock`. */
  private def cssVar(t: Tone): Option[String] = Option.when(Themed.contains(t))(s"--cpgc-${t.toString.toLowerCase}")

  /** Colour for SVG attributes: a `var(...)` for themed tones, a hex string for stage tints. */
  def css(t: Tone): String = cssVar(t).fold(hexString(Tints(t)))(v => s"var($v)")

  /** Stylesheet defining the themed tones for `selector` (dark values, light under the media query). */
  def styleBlock(selector: String): String =
    def decls(light: Boolean) = Themed.keys.toSeq.sortBy(_.ordinal)
      .map(t => s"${cssVar(t).get}:${hexString(hex(t, light))}").mkString(";")
    s"$selector{${decls(false)}}\n@media (prefers-color-scheme: light){$selector{${decls(true)}}}"
