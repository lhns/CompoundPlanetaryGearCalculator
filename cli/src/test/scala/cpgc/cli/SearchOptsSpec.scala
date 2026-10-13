package cpgc.cli

import com.monovore.decline.Command
import munit.FunSuite

/** The CLI maps `SearchConfig.validate` errors to decline errors, the same rules as the web UI. */
class SearchOptsSpec extends FunSuite:

  private val cmd = Command("cpgc", "test")(Search.command)

  private def errors(args: String*): String =
    cmd.parse(args.toList).fold(_.toString, _ => fail(s"expected a decline error for ${args.mkString(" ")}"))

  test("od <= 2 * ring wall is a decline error") {
    assert(errors("--od", "4", "--ring-wall", "2").contains("larger than twice the ring wall"))
    assert(errors("--od", "3", "--ring-wall", "2").contains("larger than twice the ring wall"))
  }

  test("other invalid numbers are decline errors, not exceptions") {
    assert(errors("--min-sun", "1").contains("Sun teeth"))
    assert(errors("--max-ring", "10").contains("Ring teeth"))
    assert(errors("--sun-planet-factor", "0").contains("planet/sun"))
    assert(errors("--target", "NaN").contains("finite"))
  }

  test("no sun range is required when both suns are removed") {
    assert(cmd.parse(List("--mode", "cdcp", "--sunless", "both", "--min-sun", "1")).isRight)
  }

  test("valid defaults parse") {
    assertEquals(cpgc.constraints.SearchConfig.DefaultSunPlanetFactor, 3.0)
    assert(cmd.parse(Nil).isRight)
  }
