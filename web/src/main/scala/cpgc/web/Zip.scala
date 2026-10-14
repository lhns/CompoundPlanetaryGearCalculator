package cpgc.web

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

/** Minimal store-only (no compression) ZIP writer: local headers, central directory, end record.
  * File names are UTF-8 (general-purpose bit 11); the timestamp is fixed (1980-01-01). */
object Zip:

  private val table: Array[Int] = Array.tabulate(256) { n =>
    var c = n
    for _ <- 0 until 8 do c = if (c & 1) != 0 then 0xEDB88320 ^ (c >>> 1) else c >>> 1
    c
  }

  /** CRC-32 (IEEE 802.3) of `data`, as an unsigned value. */
  def crc32(data: Array[Byte]): Long =
    var c = -1
    var i = 0
    while i < data.length do
      c = table((c ^ data(i)) & 0xFF) ^ (c >>> 8)
      i += 1
    (~c).toLong & 0xFFFFFFFFL

  private val DosDate = 0x0021 // 1980-01-01
  private val Utf8Flag = 0x0800

  private final class Out:
    val b = ByteArrayOutputStream()
    def size: Int = b.size
    def u16(v: Int): Unit = { b.write(v & 0xFF); b.write((v >>> 8) & 0xFF) }
    def u32(v: Long): Unit = { u16((v & 0xFFFF).toInt); u16(((v >>> 16) & 0xFFFF).toInt) }
    def bytes(a: Array[Byte]): Unit = b.write(a, 0, a.length)

  def store(files: Seq[(String, Array[Byte])]): Array[Byte] =
    val out = Out()
    // (name bytes, crc, size, local header offset)
    val entries = files.map { (name, data) =>
      val nameBytes = name.getBytes(StandardCharsets.UTF_8)
      val crc       = crc32(data)
      val offset    = out.size
      out.u32(0x04034b50L); out.u16(20); out.u16(Utf8Flag); out.u16(0)
      out.u16(0); out.u16(DosDate)
      out.u32(crc); out.u32(data.length); out.u32(data.length)
      out.u16(nameBytes.length); out.u16(0)
      out.bytes(nameBytes); out.bytes(data)
      (nameBytes, crc, data.length, offset)
    }
    val dirStart = out.size
    entries.foreach { (nameBytes, crc, size, offset) =>
      out.u32(0x02014b50L); out.u16(20); out.u16(20); out.u16(Utf8Flag); out.u16(0)
      out.u16(0); out.u16(DosDate)
      out.u32(crc); out.u32(size); out.u32(size)
      out.u16(nameBytes.length); out.u16(0); out.u16(0); out.u16(0); out.u16(0)
      out.u32(0); out.u32(offset)
      out.bytes(nameBytes)
    }
    val dirSize = out.size - dirStart
    out.u32(0x06054b50L); out.u16(0); out.u16(0)
    out.u16(entries.size); out.u16(entries.size)
    out.u32(dirSize); out.u32(dirStart); out.u16(0)
    out.b.toByteArray
