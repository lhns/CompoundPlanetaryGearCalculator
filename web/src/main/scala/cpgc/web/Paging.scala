package cpgc.web

/** Pure page arithmetic for the results table. Pages are 0-based; a page size
  * of `None` means "All" (one page). */
object Paging:

  def pageCount(total: Int, size: Option[Int]): Int =
    size match
      case Some(s) if total > 0 => (total + s.max(1) - 1) / s.max(1)
      case _                    => 1

  def clamp(page: Int, total: Int, size: Option[Int]): Int =
    page.max(0).min(pageCount(total, size) - 1)

  /** Half-open row range `[from, until)` shown on `page` (clamped). */
  def range(page: Int, total: Int, size: Option[Int]): (Int, Int) =
    size match
      case None => (0, total)
      case Some(s) =>
        val from = clamp(page, total, size) * s.max(1)
        (from, math.min(total, from + s.max(1)))

  /** Page that keeps the first visible row on screen after the page size changes. */
  def pageForSizeChange(page: Int, total: Int, oldSize: Option[Int], newSize: Option[Int]): Int =
    val first = range(page, total, oldSize)._1
    newSize match
      case None    => 0
      case Some(s) => clamp(first / s.max(1), total, newSize)

  /** "Showing a–b of N" (1-based, inclusive); "Showing 0 of 0" when empty. */
  def label(page: Int, total: Int, size: Option[Int]): String =
    if total == 0 then "Showing 0 of 0"
    else
      val (from, until) = range(page, total, size)
      s"Showing ${from + 1}–$until of $total"
