package cpgc.web

import munit.FunSuite

class PagingSpec extends FunSuite:
  private val ten = Some(10)

  test("empty results: one page, empty range"):
    assertEquals(Paging.pageCount(0, ten), 1)
    assertEquals(Paging.range(0, 0, ten), (0, 0))
    assertEquals(Paging.label(0, 0, ten), "Showing 0 of 0")

  test("exact multiple"):
    assertEquals(Paging.pageCount(30, ten), 3)
    assertEquals(Paging.range(2, 30, ten), (20, 30))
    assertEquals(Paging.label(2, 30, ten), "Showing 21–30 of 30")

  test("partial last page"):
    assertEquals(Paging.pageCount(25, ten), 3)
    assertEquals(Paging.range(2, 25, ten), (20, 25))
    assertEquals(Paging.label(1, 25, ten), "Showing 11–20 of 25")

  test("All is a single page"):
    assertEquals(Paging.pageCount(25, None), 1)
    assertEquals(Paging.range(5, 25, None), (0, 25))
    assertEquals(Paging.label(0, 25, None), "Showing 1–25 of 25")

  test("clamping after results shrink and for out-of-range input"):
    assertEquals(Paging.clamp(4, 25, ten), 2)
    assertEquals(Paging.clamp(-1, 25, ten), 0)
    assertEquals(Paging.clamp(Int.MaxValue, 25, ten), 2)
    assertEquals(Paging.range(4, 25, ten), (20, 25))
    assertEquals(Paging.clamp(3, 0, ten), 0)

  test("rows change keeps the first visible row"):
    // page 2 of 10 starts at row 20 -> with 25 rows per page that is page 0
    assertEquals(Paging.pageForSizeChange(2, 100, ten, Some(25)), 0)
    // page 3 of 10 starts at row 30 -> 25 rows per page: row 30 is on page 1
    assertEquals(Paging.pageForSizeChange(3, 100, ten, Some(25)), 1)
    assertEquals(Paging.pageForSizeChange(3, 100, ten, None), 0)
    assertEquals(Paging.pageForSizeChange(0, 100, None, ten), 0)
    assertEquals(Paging.pageForSizeChange(7, 25, ten, Some(5)), 4) // stale page clamps first (row 20)
