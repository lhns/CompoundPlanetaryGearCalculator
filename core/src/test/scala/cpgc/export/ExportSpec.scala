package cpgc.`export`

import cpgc.Calculator
import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.CompoundDesign
import munit.FunSuite

class ExportSpec extends FunSuite:
  import ExportFixtures.*

  private def csvCells(d: CompoundDesign): Vector[String] =
    Csv.renderRow(d).split(",", -1).toVector

  // -- P1: sun-less stages never show a sun -----------------------------------

  test("P1 CSV: sun cells are empty exactly for absent suns") {
    // columns: mode,ratio,efficiency,planets,sun1,planet1,ring1,module1,sun2,...
    def suns(d: CompoundDesign) = (csvCells(d)(4), csvCells(d)(8))
    assertEquals(suns(standard), ("25", "26"))
    assertEquals(suns(nss), ("25", ""))
    assertEquals(suns(s1Absent), ("", "26"))
    assertEquals(suns(sunless), ("", ""))
  }

  test("P1 JSON: sun is null exactly for absent suns") {
    assert(Json.renderOne(standard).contains(""""stage1":{"sun":25"""))
    assert(Json.renderOne(nss).contains(""""stage2":{"sun":null"""))
    assert(Json.renderOne(s1Absent).contains(""""stage1":{"sun":null"""))
    val sl = Json.renderOne(sunless)
    assert(sl.contains(""""stage1":{"sun":null""") && sl.contains(""""stage2":{"sun":null"""))
  }

  test("P1 Pretty: absent suns show a dash, never the virtual tooth count") {
    val t = Pretty.table(Seq(sunless)).linesIterator.toVector
    val cells = t(3).split('│').map(_.trim).filter(_.nonEmpty)
    assertEquals(cells(4), "—")
    assertEquals(cells(8), "—")
  }

  test("P1 OpenSCAD: no sun gear for sun-less stages") {
    def suns(d: CompoundDesign) = OpenScad.emit(d).linesIterator.count(_.contains("sun_gear"))
    assertEquals(suns(standard), 2)
    assertEquals(suns(nss), 1)
    assertEquals(suns(s1Absent), 1)
    assertEquals(suns(sunless), 0)
    assertEquals(suns(single), 1)
    // the virtual sun's tooth count must not appear as a gear
    assert(!OpenScad.emit(sunless).contains("tooth_number=26"))
    assert(!OpenScad.emit(sunless).contains("tooth_number=25"))
  }

  // -- traceability -----------------------------------------------------------

  test("CSV: original columns stay put, topology columns are appended") {
    val head = Csv.render(Seq(standard)).linesIterator.next()
    assert(head.startsWith("mode,ratio,efficiency,planets,sun1,planet1,ring1,module1,sun2,planet2,ring2,module2,"))
    assertEquals(head.split(',').toVector.drop(12),
      Vector("input", "output", "fixed", "firstCoupling", "secondCoupling", "absentSuns"))
    assertEquals(csvCells(standard).drop(12), Vector("S1", "R2", "R1", "CarrierToCarrier", "PlanetStack", ""))
    assertEquals(csvCells(nss).last, "S2")
    assertEquals(csvCells(sunless).last, "S1;S2")
    assertEquals(csvCells(s1Absent).last, "S1")
    assertEquals(csvCells(single).drop(12), Vector("S1", "C1", "R1", "", "", ""))
    for (_, d) <- all do assertEquals(csvCells(d).length, 18)
  }

  test("JSON: topology object") {
    val j = Json.renderOne(sunless)
    assert(j.contains(""""topology":{"modeName":"CDCP (sun-less)","input":"C1","output":"R2","fixed":"R1","""), j)
    assert(j.contains(""""firstCoupling":"CarrierToCarrier","secondCoupling":"PlanetStack","absentSuns":["S1","S2"]}"""), j)
    val s = Json.renderOne(single)
    assert(s.contains(""""firstCoupling":null,"secondCoupling":null,"absentSuns":[]"""), s)
    assert(Json.renderOne(standard).contains(""""modeName":"Standard""""))
  }

  test("Pretty: layout and absent-sun columns") {
    val t = Pretty.table(Seq(nss, single))
    assert(t.contains("Layout") && t.contains("No sun"))
    assert(t.contains("S1→R2 | R1 fixed | CarrierToCarrier+PlanetStack"), t)
    assert(t.contains("S1→C1 | R1 fixed"), t)
  }

  // -- P4: table borders ------------------------------------------------------

  test("P4 Pretty: every line has the same width and the separator uses ├ ┼ ┤") {
    val lines = Pretty.table(Seq(standard, nss, single)).linesIterator.toVector
    assertEquals(lines.map(_.length).distinct.size, 1, lines.mkString("\n"))
    assert(lines(0).startsWith("┌") && lines(0).endsWith("┐"))
    assert(lines(2).startsWith("├") && lines(2).endsWith("┤") && lines(2).contains("┼"))
    assert(!lines(2).contains("├─┼"))
    assertEquals(lines(1).count(_ == '│'), lines(2).count(_ == '┼') + 2)
    assert(lines.last.startsWith("└") && lines.last.endsWith("┘"))
  }

  test("P4 Pretty: golden") {
    assertEquals(Pretty.table(Seq(sunless)), goldenPretty.filter(_ != (13: Char)))
  }

  private val goldenPretty: String =
    """┌─────────────────┬────────┬───────┬───┬────┬────┬────┬───────┬────┬────┬────┬───────┬─────────────────────────────────────────────────┬────────┐
      |│ Mode            │ Ratio  │ Eff   │ N │ S1 │ P1 │ R1 │ M1    │ S2 │ P2 │ R2 │ M2    │ Layout                                          │ No sun │
      |├─────────────────┼────────┼───────┼───┼────┼────┼────┼───────┼────┼────┼────┼───────┼─────────────────────────────────────────────────┼────────┤
      |│ CDCP (sun-less) │ 12.500 │ 0.900 │ 3 │ —  │ 11 │ 47 │ 2.000 │ —  │ 10 │ 46 │ 2.000 │ C1→R2 | R1 fixed | CarrierToCarrier+PlanetStack │ S1;S2  │
      |└─────────────────┴────────┴───────┴───┴────┴────┴────┴───────┴────┴────┴────┴───────┴─────────────────────────────────────────────────┴────────┘""".stripMargin

  // -- P6: non-finite numbers -------------------------------------------------

  test("P6 Numeric.fixed: NaN and infinities are not printed as 0") {
    assertEquals(Numeric.fixed(Double.NaN, 3), "NaN")
    assertEquals(Numeric.fixed(Double.PositiveInfinity, 3), "Inf")
    assertEquals(Numeric.fixed(Double.NegativeInfinity, 3), "-Inf")
  }

  test("P6 Numeric.fixed: no Long overflow, rounding and sign") {
    assert(Numeric.fixed(1e300, 2).startsWith("10000"))
    assert(Numeric.fixed(-1e300, 2).startsWith("-10000"))
    assertEquals(Numeric.fixed(1e19, 3), "10000000000000000000.000")
    assertEquals(Numeric.fixed(12.3456789, 4), "12.3457")
    assertEquals(Numeric.fixed(-0.0004, 3), "0.000")
    assertEquals(Numeric.fixed(0.5, 3), "0.500")
  }

  test("P6 CSV/Pretty/JSON agree on a NaN efficiency") {
    val d = standard.copy(efficiency = Double.NaN)
    assertEquals(csvCells(d)(2), "NaN")
    assert(Pretty.table(Seq(d)).contains("NaN"))
    assert(Json.renderOne(d).contains(""""efficiency":null"""))
    val inf = standard.copy(efficiency = Double.PositiveInfinity)
    assertEquals(csvCells(inf)(2), "Inf")
    assert(Json.renderOne(inf).contains(""""efficiency":null"""))
  }

  test("determinism: the same config renders identical CSV") {
    val cfg = SearchConfig(
      topology = Topology.singleStage, sunTeeth = TeethBounds(10, 20), planetTeeth = TeethBounds(10, 14),
      ringTeeth = TeethBounds(40, 60), planets = TeethBounds(3, 3))
    assertEquals(Csv.render(Calculator.search(cfg).take(20)), Csv.render(Calculator.search(cfg).take(20)))
  }
