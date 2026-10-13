package cpgc.search

import munit.FunSuite
import scala.util.Random

class LinAlgSpec extends FunSuite:

  private def matvec(a: Array[Array[Double]], x: Array[Double]): Array[Double] =
    val n = x.length
    Array.tabulate(n) { i =>
      var sum = 0.0
      var j = 0
      while j < n do
        sum += a(i)(j) * x(j)
        j += 1
      sum
    }

  private def maxAbsDiff(a: Array[Double], b: Array[Double]): Double =
    a.zip(b).map((x, y) => math.abs(x - y)).max

  test("solve handles a hand-checked 3x3 system exactly") {
    // Solve:
    //  2x + y - z = 8
    // -3x - y + 2z = -11
    // -2x + y + 2z = -3
    // Expected x = 2, y = 3, z = -1
    val a = Array(
      Array(2.0,  1.0, -1.0),
      Array(-3.0, -1.0, 2.0),
      Array(-2.0, 1.0,  2.0)
    )
    val b = Array(8.0, -11.0, -3.0)
    val Some(x) = LinAlg.solve(a, b): @unchecked
    assertEqualsDouble(x(0),  2.0, 1e-9)
    assertEqualsDouble(x(1),  3.0, 1e-9)
    assertEqualsDouble(x(2), -1.0, 1e-9)
  }

  test("solve handles a hand-checked 5x5 system exactly") {
    // Identity-ish system: A = I, b = [1,2,3,4,5], x must equal b.
    val n = 5
    val a = Array.tabulate(n, n)((i, j) => if i == j then 1.0 else 0.0)
    val b = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val Some(x) = LinAlg.solve(a, b): @unchecked
    for i <- 0 until n do
      assertEqualsDouble(x(i), b(i), 1e-12)
  }

  test("solve agrees with A·x = b for 200 random 5x5 systems") {
    val rng = new Random(seed = 42)  // deterministic
    var checked = 0
    var i = 0
    while i < 200 do
      val n = 5
      val a = Array.fill(n, n)(rng.between(-10.0, 10.0))
      val xTrue = Array.fill(n)(rng.between(-5.0, 5.0))
      val b = matvec(a, xTrue)
      LinAlg.solve(a, b) match
        case Some(xSolved) =>
          val residual = matvec(a, xSolved)
          val err = maxAbsDiff(residual, b)
          assert(err < 1e-7, s"sample $i: residual $err exceeds 1e-7 (seed=42)")
          checked += 1
        case None =>
          // accidentally singular — skip
      i += 1
    assert(checked > 190, s"only $checked / 200 samples were non-singular — RNG too unfriendly")
  }

  test("solve returns None for an exactly singular system (zero row)") {
    val a = Array(
      Array(1.0, 2.0, 3.0),
      Array(0.0, 0.0, 0.0),  // zero row
      Array(4.0, 5.0, 6.0)
    )
    val b = Array(1.0, 0.0, 1.0)
    assertEquals(LinAlg.solve(a, b), None)
  }

  test("solve returns None for a singular system (linearly dependent rows)") {
    val a = Array(
      Array(1.0, 2.0, 3.0),
      Array(2.0, 4.0, 6.0),  // 2× the first row
      Array(1.0, 0.0, 1.0)
    )
    val b = Array(1.0, 2.0, 0.5)
    assertEquals(LinAlg.solve(a, b), None)
  }

  test("solve returns None when the largest pivot falls below eps") {
    val a = Array(
      Array(1e-15, 1.0),
      Array(0.0,   2e-15)
    )
    val b = Array(1.0, 1.0)
    assertEquals(LinAlg.solve(a, b, eps = 1e-12), None)
  }
