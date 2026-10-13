package cpgc.search

/** Tiny dependency-free linear-algebra helper for the role-assignment solver.
  * The matrices we feed it are at most 5×5 dense doubles — the kinematic
  * constraints for a compound planetary gearbox — so we don't need a real
  * BLAS, and we don't want a dependency for it.
  */
object LinAlg:

  /** Solve `A · x = b` for `x` via partial-pivoting Gauss elimination with
    * back-substitution.
    *
    * Returns `None` when `A` is singular within tolerance — i.e. when the
    * largest pivot found in some column has `|value| < eps`. For the
    * kinematic system this signals "this topology has no well-defined ratio
    * at these tooth counts" (e.g. choosing the same body as Input and Fixed
    * would produce two identical pin rows, making the system singular).
    *
    * The input matrices are mutated in place — pass a fresh copy if you need
    * to reuse them.
    */
  def solve(a: Array[Array[Double]], b: Array[Double], eps: Double = 1e-12): Option[Array[Double]] =
    solveDetailed(a, b, eps).map(_._1)

  /** Like `solve` but also returns the smallest pivot magnitude encountered
    * during forward elimination — a cheap stand-in for the matrix's condition
    * number. Callers wanting to flag near-singular solutions (which yield
    * noisy results even though they pass the strict `eps` cutoff) can compare
    * this value against a looser threshold. */
  def solveDetailed(a: Array[Array[Double]], b: Array[Double], eps: Double = 1e-12): Option[(Array[Double], Double)] =
    val n = b.length
    require(a.length == n, s"matrix row count ${a.length} != b.length $n")
    a.foreach(row => require(row.length == n, s"matrix not square: row of length ${row.length} vs $n"))

    // Working copies so the caller's arrays remain untouched.
    val m = Array.tabulate(n)(i => a(i).clone())
    val r = b.clone()

    // Forward elimination with partial pivoting.
    var col = 0
    var minPivot = Double.PositiveInfinity
    while col < n do
      // Find the row with the largest absolute pivot in this column.
      var pivotRow = col
      var pivotMag = math.abs(m(col)(col))
      var k = col + 1
      while k < n do
        val mag = math.abs(m(k)(col))
        if mag > pivotMag then
          pivotMag = mag
          pivotRow = k
        k += 1

      if pivotMag < eps then return None  // singular within tolerance.
      if pivotMag < minPivot then minPivot = pivotMag

      // Swap pivot row into position.
      if pivotRow != col then
        val tmp = m(col); m(col) = m(pivotRow); m(pivotRow) = tmp
        val tmpR = r(col); r(col) = r(pivotRow); r(pivotRow) = tmpR

      // Eliminate column `col` from rows below.
      val pivot = m(col)(col)
      var i = col + 1
      while i < n do
        val factor = m(i)(col) / pivot
        if factor != 0.0 then
          var j = col
          while j < n do
            m(i)(j) -= factor * m(col)(j)
            j += 1
          r(i) -= factor * r(col)
        i += 1

      col += 1

    // Back-substitution.
    val x = new Array[Double](n)
    var i = n - 1
    while i >= 0 do
      var sum = r(i)
      var j = i + 1
      while j < n do
        sum -= m(i)(j) * x(j)
        j += 1
      x(i) = sum / m(i)(i)
      i -= 1

    Some((x, minPivot))

  /** Allocation-free variant for the solver hot path: solves the `n×n` system
    * stored row-major in `m` (length `n·n`) against `r` (length `n`), **in
    * place**. On success the solution is left in `r` and the smallest pivot
    * magnitude is returned; on a singular matrix (pivot < `eps`) it returns
    * `-1.0` and the contents of `m` / `r` are unspecified. Same elimination as
    * `solveDetailed`. */
  def solveFlat(m: Array[Double], r: Array[Double], n: Int, eps: Double = 1e-12): Double =
    var minPivot = Double.PositiveInfinity
    var col = 0
    while col < n do
      var pivotRow = col
      var pivotMag = math.abs(m(col * n + col))
      var k = col + 1
      while k < n do
        val mag = math.abs(m(k * n + col))
        if mag > pivotMag then
          pivotMag = mag
          pivotRow = k
        k += 1
      if pivotMag < eps || pivotMag.isNaN then return -1.0
      if pivotMag < minPivot then minPivot = pivotMag
      if pivotRow != col then
        var j = 0
        while j < n do
          val t = m(col * n + j); m(col * n + j) = m(pivotRow * n + j); m(pivotRow * n + j) = t
          j += 1
        val tr = r(col); r(col) = r(pivotRow); r(pivotRow) = tr
      val pivot = m(col * n + col)
      var i = col + 1
      while i < n do
        val factor = m(i * n + col) / pivot
        if factor != 0.0 then
          var j = col
          while j < n do
            m(i * n + j) -= factor * m(col * n + j)
            j += 1
          r(i) -= factor * r(col)
        i += 1
      col += 1
    // Back-substitution, overwriting r with the solution.
    var i = n - 1
    while i >= 0 do
      var sum = r(i)
      var j = i + 1
      while j < n do
        sum -= m(i * n + j) * r(j)
        j += 1
      r(i) = sum / m(i * n + i)
      i -= 1
    minPivot
