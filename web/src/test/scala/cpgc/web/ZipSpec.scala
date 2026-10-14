package cpgc.web

import java.nio.charset.StandardCharsets.UTF_8
import munit.FunSuite

class ZipSpec extends FunSuite:

  test("CRC-32 check value"):
    assertEquals(Zip.crc32("123456789".getBytes(UTF_8)), 0xCBF43926L)
    assertEquals(Zip.crc32(Array.empty[Byte]), 0L)

  private def u16(a: Array[Byte], p: Int): Int = (a(p) & 0xFF) | ((a(p + 1) & 0xFF) << 8)
  private def u32(a: Array[Byte], p: Int): Long = u16(a, p).toLong | (u16(a, p + 2).toLong << 16)

  test("store: archive parses back and contents round-trip (non-ASCII name)"):
    val files = Seq("a.scad" -> "use <gears.scad>;\n".getBytes(UTF_8), "Zähne ü.scad" -> Array.tabulate[Byte](300)(i => (i * 7).toByte))
    val z = Zip.store(files)

    val eocd = z.length - 22
    assertEquals(u32(z, eocd), 0x06054b50L)
    assertEquals(u16(z, eocd + 8), 2)
    assertEquals(u16(z, eocd + 10), 2)
    val dirSize  = u32(z, eocd + 12).toInt
    val dirStart = u32(z, eocd + 16).toInt
    assertEquals(dirStart + dirSize, eocd)

    var p = dirStart
    for (name, data) <- files do
      assertEquals(u32(z, p), 0x02014b50L)
      assertEquals(u16(z, p + 8) & 0x0800, 0x0800)
      assertEquals(u16(z, p + 10), 0) // stored
      assertEquals(u32(z, p + 16), Zip.crc32(data))
      assertEquals(u32(z, p + 20), data.length.toLong)
      assertEquals(u32(z, p + 24), data.length.toLong)
      val nameLen = u16(z, p + 28)
      val local   = u32(z, p + 42).toInt
      assertEquals(new String(z, p + 46, nameLen, UTF_8), name)
      p += 46 + nameLen
      // local header
      assertEquals(u32(z, local), 0x04034b50L)
      assertEquals(u32(z, local + 14), Zip.crc32(data))
      assertEquals(u16(z, local + 26), nameLen)
      val start = local + 30 + nameLen + u16(z, local + 28)
      assertEquals(z.slice(start, start + data.length).toSeq, data.toSeq)
    assertEquals(p, eocd)
