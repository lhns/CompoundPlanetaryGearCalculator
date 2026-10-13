package cpgc.`export`

import java.util.Locale
import munit.FunSuite

/** P2: every export is locale-independent. */
class ExportLocaleSpec extends FunSuite:
  import ExportFixtures.*

  private def renderAll(): Vector[String] =
    val ds = all.map(_._2)
    Vector(Csv.render(ds), Json.render(ds), Pretty.table(ds)) ++ ds.map(OpenScad.emit(_))

  test("P2 exports are identical under de-DE and en-US defaults") {
    val saved = Locale.getDefault
    try
      Locale.setDefault(Locale.US)
      val us = renderAll()
      Locale.setDefault(Locale.GERMANY)
      val de = renderAll()
      us.zip(de).foreach((a, b) => assertEquals(b, a))
      assert(!de(3).matches("(?s).*\\d,\\d.*"), "decimal comma in OpenSCAD")
    finally Locale.setDefault(saved)
  }
