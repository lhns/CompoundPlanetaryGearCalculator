package cpgc.web

import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, Topology}
import cpgc.search.Scorer.SortBy
import cpgc.web.UiLogic.parseFinite

// URL query handling: parse the address-bar state into the form, and mirror the form back.
object UrlState:

  private def hexVal(c: Char): Int = Character.digit(c, 16)

  /** Percent-decode (UTF-8); `+` is a space. Malformed escapes stay literal. */
  def percentDecode(s: String): String =
    if !s.contains('%') && !s.contains('+') then s
    else
      val out = new java.io.ByteArrayOutputStream()
      def put(str: String): Unit =
        val b = str.getBytes("UTF-8")
        out.write(b, 0, b.length)
      var i = 0
      while i < s.length do
        val c = s.charAt(i)
        if c == '%' && i + 2 < s.length && hexVal(s.charAt(i + 1)) >= 0 && hexVal(s.charAt(i + 2)) >= 0 then
          out.write(hexVal(s.charAt(i + 1)) * 16 + hexVal(s.charAt(i + 2)))
          i += 3
        else if c == '+' then { out.write(' '); i += 1 }
        else if Character.isHighSurrogate(c) && i + 1 < s.length then { put(s.substring(i, i + 2)); i += 2 }
        else { put(c.toString); i += 1 }
      new String(out.toByteArray, "UTF-8")

  /** Percent-encode everything except unreserved characters and `,`. */
  def percentEncode(s: String): String =
    val sb = new StringBuilder
    for b <- s.getBytes("UTF-8") do
      val v = b & 0xff
      val c = v.toChar
      if (v < 128 && c.isLetterOrDigit) || "-._~,".indexOf(c) >= 0 then sb += c
      else sb ++= f"%%$v%02X"
    sb.toString

  /** Query string → decoded name/value pairs. */
  private def pairs(query: String): Vector[(String, String)] =
    query.stripPrefix("?").split('&').toVector.filter(_.nonEmpty).map { kv =>
      val i = kv.indexOf('=')
      if i < 0 then (percentDecode(kv), "") else (percentDecode(kv.take(i)), percentDecode(kv.drop(i + 1)))
    }

  /** One URL parameter value by exact name, percent-decoded
    * (`?target=53%2C12` → `Some("53,12")`). The first occurrence wins. */
  def urlParam(query: String, name: String): Option[String] =
    pairs(query).collectFirst { case (k, v) if k == name => v }

  private val RangeRe = """(-?\d+)(?:-(-?\d+))?""".r

  /** Parse `lo-hi`, or a single value `n` as the degenerate range `(n, n)`. */
  def parseRange(s: String): Option[(Int, Int)] = s.trim match
    case RangeRe(lo, null) => lo.toIntOption.map(l => (l, l))
    case RangeRe(lo, hi)   => for l <- lo.toIntOption; h <- hi.toIntOption yield (l, h)
    case _                 => None

  private def modeParam(v: String): Option[Topology] = v.trim.toLowerCase match
    case "standard"                 => Some(Topology.standardCompound)
    case "single"                   => Some(Topology.singleStage)
    case "cdcp"                     => Some(Topology.cdcpCompound)
    case "nss"                      => Some(Topology.nssCompound)
    case "cdcp-sunless" | "sunless" => Some(Topology.sunlessCdcpCompound)
    case _                          => None

  /** `?mode=` names a base layout; `coupling1/2`, `input/output/fixed` (all
    * three) and `absent=S1,S2` then override it. An override that does not
    * give a legal topology is skipped. */
  def initialTopology(query: String): Topology =
    val base = urlParam(query, "mode").flatMap(modeParam).getOrElse(Topology.standardCompound)
    def tryBuild(t: => Topology, fallback: Topology): Topology =
      try t catch case _: IllegalArgumentException => fallback

    def couplingParam(name: String): Option[Coupling] =
      urlParam(query, name).flatMap(v => Coupling.values.find(_.toString == v))
    val c1 = couplingParam("coupling1")
    // the legacy `?coupling=` flag still sets just the second coupling
    val c2 = couplingParam("coupling2").orElse(couplingParam("coupling"))
    val withCouplings =
      if base.stages != 2 || (c1.isEmpty && c2.isEmpty) then base
      else tryBuild(base.copy(firstCoupling = c1.getOrElse(base.firstCoupling),
                              secondCoupling = c2.getOrElse(base.secondCoupling)), base)

    def bodyParam(name: String): Option[Body] =
      urlParam(query, name).flatMap(v => Body.values.find(_.toString == v))
    val absentParam: Option[Set[Body]] = urlParam(query, "absent").map(
      _.split(',').iterator.map(_.trim).flatMap(n => Body.values.find(_.toString == n)).toSet)

    val pinned: Option[Map[Body, BodyRole]] = (bodyParam("input"), bodyParam("output"), bodyParam("fixed")) match
      case (Some(in), Some(out), Some(fx)) if Set(in, out, fx).size == 3 =>
        Some(Map(in -> BodyRole.Input, out -> BodyRole.Output, fx -> BodyRole.Fixed))
      case _ => None

    if pinned.isEmpty && absentParam.isEmpty then withCouplings
    else
      def wasAbsent(b: Body) = withCouplings.roles.get(b).contains(BodyRole.Absent)
      def absent(b: Body)    = absentParam.fold(wasAbsent(b))(_.contains(b))
      val roles = withCouplings.roles.map { (b, r) =>
        b -> pinned.flatMap(_.get(b)).getOrElse {
          if (b == Body.S1 || b == Body.S2) && absent(b) then BodyRole.Absent
          else if pinned.isEmpty && r != BodyRole.Absent then r // only `absent` given: keep the pins
          else BodyRole.Free
        }
      }
      tryBuild(withCouplings.copy(roles = roles), withCouplings)

  def initialSort(query: String): SortBy =
    urlParam(query, "sort").flatMap(SortBy.parse).getOrElse(SortBy.BestFit)

  /** Initial form values from a query string:
    *   ?sun=10-40&planet=10-18&ring=40-120&planets=3-4&od=150&minmod=0.5&target=53.12
    *   &ringwall=2&clearance=0.5&phased=1
    * Falls back to the defaults when a param is absent or unparseable. */
  def initialForm(query: String): FormValues =
    def range(name: String, d: (Int, Int)) = urlParam(query, name).flatMap(parseRange).getOrElse(d)
    def num(name: String, d: Double)       = urlParam(query, name).flatMap(parseFinite).getOrElse(d)
    val sunR     = range("sun", (10, 40))
    val planetR  = range("planet", (10, 18))
    val ringR    = range("ring", (40, 120))
    val planetsR = range("planets", (3, 4))
    FormValues(
      topology        = initialTopology(query),
      sunMin = sunR._1, sunMax = sunR._2,
      planetMin = planetR._1, planetMax = planetR._2,
      ringMin = ringR._1, ringMax = ringR._2,
      planetsMin = planetsR._1, planetsMax = planetsR._2,
      od              = num("od", 150.0),
      minModule       = num("minmod", 0.5),
      sunPlanetFactor = urlParam(query, "spfact").getOrElse(""),
      target          = urlParam(query, "target").getOrElse("53.12"),
      phasedPinions   = urlParam(query, "phased").exists(v => v == "1" || v == "true"),
      ringWall        = num("ringwall", SearchConfig.DefaultRingWall),
      clearance       = num("clearance", SearchConfig.DefaultPlanetClearance)
    )

  private def fmtNum(d: Double): String =
    if d == math.rint(d) && math.abs(d) < 1e15 then d.toLong.toString else d.toString

  /** Query string (without `?`) that `initialForm` / `initialTopology` /
    * `initialSort` read back to the same state: the URL mirrors the page. */
  def toQuery(f: FormValues, sort: SortBy): String =
    val t = f.topology
    val topo: Vector[(String, String)] =
      t.legacyMode match
        case Some(m) => Vector("mode" -> (if t.stages == 1 then "single" else m.toString.toLowerCase))
        case None =>
          val base =
            if t == Topology.sunlessCdcpCompound then Vector("mode" -> "cdcp-sunless")
            else Vector("mode" -> (if t.stages == 1 then "single" else "standard"))
          base ++
            (if t.stages == 2 then Vector("coupling1" -> t.firstCoupling.toString, "coupling2" -> t.secondCoupling.toString)
             else Vector.empty) ++
            Vector("input" -> t.input.toString, "output" -> t.output.toString, "fixed" -> t.fixed.toString,
                   "absent" -> Body.values.filter(b => t.roles.get(b).contains(BodyRole.Absent)).mkString(","))
    val rest = Vector(
      "sun" -> s"${f.sunMin}-${f.sunMax}",
      "planet" -> s"${f.planetMin}-${f.planetMax}",
      "ring" -> s"${f.ringMin}-${f.ringMax}",
      "planets" -> s"${f.planetsMin}-${f.planetsMax}",
      "od" -> fmtNum(f.od),
      "minmod" -> fmtNum(f.minModule),
      "ringwall" -> fmtNum(f.ringWall),
      "clearance" -> fmtNum(f.clearance),
      "target" -> f.target,
      "spfact" -> f.sunPlanetFactor,
      "sort" -> sort.toString.toLowerCase
    ) ++ (if f.phasedPinions then Vector("phased" -> "1") else Vector.empty)
    (topo ++ rest).map((k, v) => s"$k=${percentEncode(v)}").mkString("&")
