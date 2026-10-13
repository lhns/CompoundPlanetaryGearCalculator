package cpgc.web

import com.raquo.laminar.api.L.*
import com.raquo.laminar.api.L.svg as s
import cpgc.constraints.{BodyRole, Coupling, Topology}
import cpgc.domain.{CompoundDesign, Mode, Stage}
import cpgc.kinematics.{BodyAngles, Kinematics}
import cpgc.web.GearboxScene.{Gear, StageScene}
import cpgc.web.Palette.Tone
import cpgc.web.SchematicLayout.rotateXform

/** Animated SVG schematic. Static layout is rebuilt only when the selected
  * design changes; the per-frame `phase` signal only updates `transform`
  * attributes — Laminar updates strings in place, not the whole SVG.
  */
object Schematic:

  // One palette for both views (`Palette`); themed tones are CSS variables defined on the <svg> by `defs()`.
  private val CouplingColors = Map(1 -> Palette.css(Tone.Cpl1), 2 -> Palette.css(Tone.Cpl2))
  private val StyleTag = com.raquo.laminar.tags.SvgTag[org.scalajs.dom.SVGElement]("style")
  private val CouplingStyle = Palette.styleBlock(".cpgc-schem")
  // Theme-aware colours come from the page's CSS variables (index.html).
  private val TextColor    = "var(--svg-text)"
  private val SubText      = "var(--svg-sub)"
  private val InputColor   = Palette.css(Tone.Input)   // green accent — input body
  private val OutputColor  = Palette.css(Tone.Output)  // blue accent — output body
  private val FixedColor   = Palette.css(Tone.Fixed)   // red accent — grounded body
  private val HubFill      = "var(--svg-hub)"

  /** Which role a body is highlighted for. */
  private enum Role:
    case Input, Output, Fixed, Plain

  private def roleOf(r: Option[BodyRole]): Role = r match
    case Some(BodyRole.Input)  => Role.Input
    case Some(BodyRole.Output) => Role.Output
    case Some(BodyRole.Fixed)  => Role.Fixed
    case _                     => Role.Plain

  private def roleStrokeWidth(role: Role, base: Double): Double = role match
    case Role.Plain => base
    case _          => base * 2.0

  def apply(
      designSignal:           Signal[Option[CompoundDesign]],
      phaseSignal:            Signal[Double],
      searchStateSignal:      Signal[SearchState],
      searchProgressSignal:   Signal[Int],
      searchFractionSignal:   Signal[Double]
  ): SvgElement =
    s.svg(
      svgClassAttr := "cpgc-schem",
      s.viewBox := "-220 -160 440 350",
      s.width   := "100%",
      s.height  := "100%",
      svg.preserveAspectRatio := "xMidYMid meet",
      defs(),
      children <-- designSignal
        .combineWith(searchStateSignal, searchProgressSignal, searchFractionSignal)
        .map {
          // A design always renders with the topology it was found for (d.topology).
          case (Some(d), _,                       _, _) => renderDesign(d, phaseSignal)
          case (None,    SearchState.NoResults,   _, _) => Vector(placeholderNoResults())
          case (None,    SearchState.Invalid,     _, _) => Vector(placeholderInvalid())
          case (None,    _,                       n, f) => Vector(placeholderSearching(n, f))
        }
    )

  private def defs(): SvgElement =
    s.defs(
      StyleTag(CouplingStyle),
      s.radialGradient(
        s.idAttr := "sun-glow",
        s.stop(s.offsetAttr := "0%",   s.stopColor := "#fff5b5", s.stopOpacity := "0.55"),
        s.stop(s.offsetAttr := "60%",  s.stopColor := "#ffd166", s.stopOpacity := "0.35"),
        s.stop(s.offsetAttr := "100%", s.stopColor := "#ffd166", s.stopOpacity := "0.0")
      )
    )

  /** SVG `class` attribute (Laminar's `cls` is HTML-only by default for SVG
    * elements; we bind it explicitly here for the shimmer keyframe hook). */
  private val svgClassAttr =
    svg.svgAttr[String]("class", com.raquo.laminar.codecs.StringAsIsCodec, namespace = None)

  /** Width of the progress-bar track in viewBox units. The track spans
    * x ∈ [-TrackHalf, +TrackHalf], y = -12, height = 4. */
  private val TrackHalf = 160.0
  private val TrackY    = -12.0
  private val TrackH    = 4.0

  private def placeholderSearching(found: Int, elapsedFraction: Double): SvgElement =
    val label =
      if found > 0 then s"Searching designs… ($found found)"
      else "Searching designs…"
    val fillW = (2.0 * TrackHalf) * math.max(0.0, math.min(1.0, elapsedFraction))
    // Unique clipPath id so the shimmer is constrained to the track.
    val clipId = nextClipId()
    s.g(
      // Clip the shimmer rect to the track rectangle so it doesn't overflow
      // when its transform slides it past the track edges.
      s.defs(
        svg.clipPathTag(
          s.idAttr := clipId,
          s.rect(
            s.x := f"${-TrackHalf}%.1f", s.y := f"$TrackY%.1f",
            s.width := f"${2.0 * TrackHalf}%.1f", s.height := f"$TrackH%.1f"
          )
        )
      ),
      // Track (background)
      s.rect(
        s.x := f"${-TrackHalf}%.1f", s.y := f"$TrackY%.1f",
        s.width := f"${2.0 * TrackHalf}%.1f", s.height := f"$TrackH%.1f",
        s.fill := "var(--svg-track)"
      ),
      // Elapsed-fraction fill
      s.rect(
        s.x := f"${-TrackHalf}%.1f", s.y := f"$TrackY%.1f",
        s.width := f"$fillW%.1f", s.height := f"$TrackH%.1f",
        s.fill := "#58a6ff", s.fillOpacity := "0.85"
      ),
      // Shimmer overlay (CSS-animated translate). Clipped to track.
      s.g(
        clipPathAttr := s"url(#$clipId)",
        s.rect(
          svgClassAttr := "cpgc-shimmer",
          s.x := f"${-TrackHalf}%.1f", s.y := f"$TrackY%.1f",
          s.width := "60", s.height := f"$TrackH%.1f",
          s.fill := "#ffffff", s.fillOpacity := "0.35"
        )
      ),
      // Count label below the bar
      s.text(
        s.x := "0", s.y := "20",
        s.textAnchor := "middle",
        s.fill := SubText, s.fontSize := "14",
        s.fontFamily := "ui-sans-serif, system-ui",
        label
      )
    )

  private def placeholderInvalid(): SvgElement =
    s.g(
      s.text(
        s.x := "0", s.y := "0", s.textAnchor := "middle",
        s.fill := TextColor, s.fontSize := "14",
        s.fontFamily := "ui-sans-serif, system-ui",
        "Fix the highlighted input to search."
      )
    )

  private def placeholderNoResults(): SvgElement =
    s.g(
      s.text(
        s.x := "0", s.y := "-8", s.textAnchor := "middle",
        s.fill := TextColor, s.fontSize := "14",
        s.fontFamily := "ui-sans-serif, system-ui",
        "No design matches these constraints."
      ),
      s.text(
        s.x := "0", s.y := "16", s.textAnchor := "middle",
        s.fill := SubText, s.fontSize := "11",
        s.fontFamily := "ui-sans-serif, system-ui",
        "Try widening a tooth-count range or clearing the target ratio."
      )
    )

  private def renderDesign(
      d: CompoundDesign,
      phaseSignal: Signal[Double]
  ): Vector[SvgElement] =
    val topology = d.topology
    val twoStage = d.stage2.isDefined
    val (cx1, cx2) = SchematicLayout.stageCenters(twoStage)

    val angles: Signal[BodyAngles] = phaseSignal.map(Kinematics.at(d, _))
    // Gears, counts, outlines, tones and roles all come from the shared scene.
    val scene  = GearboxScene.of(d)
    val s1 = stageGroup(scene.stages(0), cx1, "Stage 1", angles)
    val s2 = scene.stages.lift(1).map(sc2 => stageGroup(sc2, cx2, "Stage 2", angles))

    val couplings =
      d.stage2.fold(Vector.empty[SvgElement]) { st2 =>
        val lay1 = SchematicLayout.stageLayout(d.stage1, cx1)
        val lay2 = SchematicLayout.stageLayout(st2, cx2)
        Vector(1 -> topology.firstCoupling, 2 -> topology.secondCoupling).flatMap { (slot, c) =>
          if c == Coupling.PlanetStack then planetLinks(d, st2, slot, angles, cx1, cx2)
          else SchematicLayout.connector(c, slot, lay1, lay2).toVector.flatMap(connectorEls(_, slot))
        }
      }

    headerBanner(d) ++ s1 ++ s2.getOrElse(Vector.empty) ++ couplings ++ footerInfo(d) ++ legend(d)

  /** Line style of a coupling's marks: slot 1 solid, slot 2 dashed, so the pair also differs without colour. */
  private def slotDash(slot: Int): String = if slot == 1 then "none" else "5 3"

  /** A weld drawn as a curve between the bodies' rims, over a background-coloured casing for contrast. */
  private def connectorEls(c: SchematicLayout.Connector, slot: Int): Vector[SvgElement] =
    val color = CouplingColors(slot)
    val pathD = f"M ${c.from._1}%.1f ${c.from._2}%.1f Q ${c.ctrl._1}%.1f ${c.ctrl._2}%.1f ${c.to._1}%.1f ${c.to._2}%.1f"
    def end(p: (Double, Double)): Vector[SvgElement] = Vector(
      s.circle(s.cx := f"${p._1}%.1f", s.cy := f"${p._2}%.1f", s.r := "4.2", s.fill := "var(--schematic-bg)"),
      s.circle(s.cx := f"${p._1}%.1f", s.cy := f"${p._2}%.1f", s.r := "3", s.fill := color)
    )
    Vector(
      s.path(s.d := pathD, s.fill := "none", s.stroke := "var(--schematic-bg)", s.strokeWidth := "5.5",
             s.strokeOpacity := "0.8", svg.strokeLineCap := "round", s.pointerEvents := "none"),
      s.path(s.d := pathD, s.fill := "none", s.stroke := color, s.strokeWidth := "2.4",
             svg.strokeDashArray := slotDash(slot), svg.strokeLineCap := "butt", s.pointerEvents := "none")
    ) ++ end(c.from) ++ end(c.to)

  /** Dashed links joining planet k of stage 1 to planet k of stage 2: one stacked pinion each.
    * Endpoints follow the carriers, so the lines turn with the planets. */
  private def planetLinks(
      d: CompoundDesign, st2: Stage, slot: Int, angles: Signal[BodyAngles], cx1: Double, cx2: Double
  ): Vector[SvgElement] =
    val lay1 = SchematicLayout.stageLayout(d.stage1, cx1)
    val lay2 = SchematicLayout.stageLayout(st2, cx2)
    val n = d.stage1.set.numPlanets
    Vector.tabulate(n) { k =>
      val ends = angles.map(b =>
        (SchematicLayout.planetPositionAt(k, n, cx1, lay1.carrierR, b.carrier1),
         SchematicLayout.planetPositionAt(k, n, cx2, lay2.carrierR, b.carrier2)))
      s.line(
        s.x1 <-- ends.map(e => f"${e._1._1}%.2f"), s.y1 <-- ends.map(e => f"${e._1._2}%.2f"),
        s.x2 <-- ends.map(e => f"${e._2._1}%.2f"), s.y2 <-- ends.map(e => f"${e._2._2}%.2f"),
        s.stroke := CouplingColors(slot), s.strokeWidth := "1.2", s.strokeOpacity := "0.8",
        svg.strokeDashArray := "2 3", s.pointerEvents := "none"
      )
    }

  /** Schematic title: the preset name when the topology is a named one, else a generic description. */
  def title(d: CompoundDesign): String =
    d.topology.legacyMode match
      case Some(Mode.SingleStage) => "Single-stage planetary"
      case Some(Mode.Standard)    => "Compound planetary (sun-driven)"
      case Some(Mode.CDCP)        => "Carrier-driven compound planetary"
      case Some(Mode.NSS)         => "Compound planetary (no second sun)"
      case None if d.topology == Topology.sunlessCdcpCompound => "Carrier-driven compound planetary (sun-less)"
      case None =>
        TopologyEdit.topologyPreset(d.topology) match
          case GearboxPreset.RingToSunCascade => "Ring-to-sun cascade compound"
          case GearboxPreset.Ravigneaux       => "Ravigneaux compound planetary"
          case GearboxPreset.Simpson          => "Simpson compound planetary"
          case _ =>
            if d.stage2.isEmpty then "Single-stage planetary (custom roles)" else "Compound planetary (custom topology)"

  private def headerBanner(d: CompoundDesign): Vector[SvgElement] =
    val title = Schematic.title(d)
    Vector(
      s.text(
        s.x := "0", s.y := "-138", s.textAnchor := "middle",
        s.fill := TextColor, s.fontSize := "13", s.fontWeight := "600",
        s.fontFamily := "ui-sans-serif, system-ui",
        title
      ),
      s.text(
        s.x := "0", s.y := "-122", s.textAnchor := "middle",
        s.fill := SubText, s.fontSize := "11",
        s.fontFamily := "ui-monospace, monospace",
        {
          val r = d.ratio.value
          val sign = if r < 0 then "−" else ""
          val arrow = if r < 0 then " ↺" else ""
          f"ratio $sign${math.abs(r)}%.3f : 1$arrow     η ${UiLogic.fmtEfficiency(d.efficiency)}     N=${UiLogic.planetCountLabel(d)}"
        }
      )
    )

  /** Legend: role markers bottom-left; one swatch per coupling in a strip under the footer. */
  private def legend(d: CompoundDesign): Vector[SvgElement] =
    val baseX = -212.0
    val baseY = 128.0 // stacked rows in the bottom-left corner, clear of the title and footer
    def row(i: Int, color: String, name: String): Vector[SvgElement] =
      val y = baseY + 12.0 * i
      Vector(
        s.circle(s.cx := f"$baseX%.2f", s.cy := f"$y%.2f", s.r := "3", s.fill := color),
        s.text(s.x := f"${baseX + 7}%.2f", s.y := f"${y + 3}%.2f",
               s.fill := SubText, s.fontSize := "9",
               s.fontFamily := "ui-monospace, monospace", name)
      )
    val roleRows = row(0, InputColor, "input") ++ row(1, OutputColor, "output") ++ row(2, FixedColor, "fixed")
    val couplingRows =
      if d.stage2.isEmpty then Vector.empty
      else
        Vector(1 -> d.topology.firstCoupling, 2 -> d.topology.secondCoupling).zipWithIndex.flatMap {
          case ((slot, c), i) =>
            val y = 166.0 + 14.0 * i
            Vector(
              s.rect(s.x := f"${baseX - 3}%.2f", s.y := f"${y - 4}%.2f", s.width := "9", s.height := "9",
                     s.fill := CouplingColors(slot), s.rx := "1.5"),
              s.text(s.x := f"${baseX + 11}%.2f", s.y := f"${y + 4}%.2f",
                     s.fill := TextColor, s.fontSize := "10.5",
                     s.fontFamily := "ui-monospace, monospace",
                     SchematicLayout.couplingLegendLabel(c))
            )
        }
    roleRows ++ couplingRows

  private def footerInfo(d: CompoundDesign): Vector[SvgElement] =
    val module1 = f"m₁=${d.stage1.module.value}%.2f mm"
    val module2 = d.stage2.fold("")(s2 => f"     m₂=${s2.module.value}%.2f mm")
    val c2c     = f"     C2C=${d.stage1.centerToCenter}%.1f mm"
    Vector(
      s.text(
        s.x := "0", s.y := "148", s.textAnchor := "middle",
        s.fill := SubText, s.fontSize := "10",
        s.fontFamily := "ui-monospace, monospace",
        s"$module1$module2$c2c"
      )
    )


  /** Counter for unique clip-path IDs. Each rendered polygon gets its own
    * clipPath so the rendered stroke can be clipped to the polygon interior. */
  private var clipIdCounter: Int = 0
  private def nextClipId(): String =
    clipIdCounter += 1
    s"cpgc-clip-$clipIdCounter"

  /** SVG `clip-path` attribute (not exposed by Laminar's default svg bindings). */
  private val clipPathAttr =
    svg.svgAttr[String]("clip-path", com.raquo.laminar.codecs.StringAsIsCodec, namespace = None)



  /** Toothed gear body from an outline path. */
  private def toothedGear(pathD: String, color: String, fillOpacity: Double, strokeWidth: Double): SvgElement =
    pathWithInsideStroke(pathD, color, fillOpacity, strokeWidth)

  /** Render a closed polygon path with an "inside" stroke. SVG has no native
    * `stroke-alignment: inside`, so we use the well-known clip-path workaround:
    * the rendered path is clipped to itself, so the half of the stroke that
    * would paint outside the polygon's geometric outline gets clipped away.
    * The visible boundary is exactly the polygon outline, regardless of stroke
    * width — and the painted stroke width must be DOUBLED to compensate for
    * losing the outside half.
    */
  private def pathWithInsideStroke(
      pathD: String, color: String, fillOpacity: Double, strokeWidth: Double
  ): SvgElement =
    val clipId = nextClipId()
    s.g(
      s.defs(
        svg.clipPathTag(
          s.idAttr := clipId,
          s.path(s.d := pathD)
        )
      ),
      s.path(
        s.d := pathD,
        s.fill := color, s.fillOpacity := fillOpacity.toString,
        s.stroke := color, s.strokeWidth := (strokeWidth * 2.0).toString,
        svg.strokeLineJoin := "round",
        clipPathAttr := s"url(#$clipId)"
      )
    )


  private def stageGroup(sc: StageScene, cx: Double, label: String, angles: Signal[BodyAngles]): Vector[SvgElement] =
    val stage        = sc.stage
    val lay          = SchematicLayout.stageLayout(stage, cx)
    val k            = lay.scale
    val mPx          = lay.mPx
    val ringOR       = lay.ringOR
    val carrierR     = k * sc.carrier.radius
    val sunPitchR    = sc.sun.fold(0.0)(g => k * g.pitchR)
    val planetPitchR = k * sc.planets.head.pitchR
    val ringPitchR   = k * sc.ring.pitchR
    val showSun      = sc.sun.isDefined
    val sunRole      = roleOf(sc.sun.flatMap(_.role))
    val ringRole     = roleOf(sc.ring.role)
    val carrierRole  = roleOf(sc.carrier.role)
    val sunSlot      = sc.sun.flatMap(_.coupling)
    val ringSlot     = sc.ring.coupling
    val carrierSlot  = sc.carrier.coupling
    val planetSlot   = sc.planets.head.coupling

    // Scene mm -> drawing units: the stage's scale and axis position (`SchematicLayout.gearCentre` / `gearPath`).
    def placed(g: Gear): String = SchematicLayout.gearPath(g, k, cx)

    val sunSig: Signal[Double]     = angles.map(a => sc.sun.fold(0.0)(GearboxScene.angle(_, a)))
    val carrierSig: Signal[Double] = angles.map(GearboxScene.carrierAngle(sc, _))
    val ringSig: Signal[Double]    = angles.map(GearboxScene.angle(sc.ring, _))

    val ringStroke   = Palette.css(sc.ring.tone)
    val ringStrokeW  = roleStrokeWidth(ringRole, 2.0)
    val sunStroke    = sc.sun.fold("")(g => Palette.css(g.tone))
    def band(slot: Option[Int], r: Double): SvgElement = slot match
      case None => s.g()
      case Some(sl) =>
        s.circle(s.cx := f"$cx%.2f", s.cy := "0", s.r := f"$r%.2f", s.fill := "none",
                 s.stroke := CouplingColors(sl), s.strokeWidth := "2.2", s.strokeOpacity := "0.9",
                 svg.strokeDashArray := slotDash(sl), s.pointerEvents := "none")

    // Dashed pitch circle
    def pitchCircle(centerX: Double, centerY: Double, radius: Double): SvgElement =
      s.circle(
        s.cx := f"$centerX%.2f", s.cy := f"$centerY%.2f",
        s.r := f"$radius%.2f",
        s.fill := "none", s.stroke := SubText, s.strokeWidth := "0.8",
        svg.strokeDashArray := "2.5 2.5", svg.strokeLineCap := "butt",
        s.strokeOpacity := "0.7"
      )

    // Translucent tint over the ring body (between its outer rim and the teeth) for a coupled ring.
    val ringTint = ringSlot.fold(s.g())(sl =>
      s.path(
        s.d := f"M ${cx + ringOR}%.2f 0 A $ringOR%.2f $ringOR%.2f 0 1 1 ${cx - ringOR}%.2f 0 A $ringOR%.2f $ringOR%.2f 0 1 1 ${cx + ringOR}%.2f 0 Z " +
               placed(sc.ring),
        svg.fillRule := "evenodd", s.fill := CouplingColors(sl), s.fillOpacity := "0.22",
        svg.transform <-- ringSig.map(rotateXform(_, cx, 0)), s.pointerEvents := "none"))

    // Ring: rotates about stage center.
    val ringEl = s.g(
      svg.transform <-- ringSig.map(rotateXform(_, cx, 0)),
      // Outer rim
      s.circle(
        s.cx := f"$cx%.2f", s.cy := "0",
        s.r := f"$ringOR%.2f",
        s.fill := "none", s.stroke := ringStroke, s.strokeWidth := f"$ringStrokeW%.2f"
      ),
      // Inner pitch circle (faint)
      pitchCircle(cx, 0, ringPitchR),
      // Involute inner tooth profile, static phase from `Phasing` so every planet-ring mesh is tooth-in-gap.
      toothedGear(placed(sc.ring), ringStroke, 0.0, 0.7)
    )

    // Role badge (IN / OUT) for the gear at (px, py), in role's colour.
    def roleBadge(role: Role, x: Double, y: Double): SvgElement = role match
      case Role.Input  =>
        s.text(s.x := f"$x%.2f", s.y := f"$y%.2f", s.textAnchor := "middle",
               s.fill := InputColor, s.fontSize := "8",
               s.fontWeight := "700",
               s.fontFamily := "ui-monospace, monospace",
               s.pointerEvents := "none",
               "IN")
      case Role.Output =>
        s.text(s.x := f"$x%.2f", s.y := f"$y%.2f", s.textAnchor := "middle",
               s.fill := OutputColor, s.fontSize := "8",
               s.fontWeight := "700",
               s.fontFamily := "ui-monospace, monospace",
               s.pointerEvents := "none",
               "OUT")
      case Role.Fixed  =>
        s.text(s.x := f"$x%.2f", s.y := f"$y%.2f", s.textAnchor := "middle",
               s.fill := FixedColor, s.fontSize := "8",
               s.fontWeight := "700",
               s.fontFamily := "ui-monospace, monospace",
               s.pointerEvents := "none",
               "FIXED")
      case Role.Plain  => s.g()

    // Sun: rotates about stage center.
    val sunEl =
      if !showSun then s.g()
      else s.g(
        svg.transform <-- sunSig.map(rotateXform(_, cx, 0)),
        s.circle(
          s.cx := f"$cx%.2f", s.cy := "0",
          s.r := f"${(sunPitchR + mPx) * 1.35}%.2f",
          s.fill := "url(#sun-glow)"
        ),
        pitchCircle(cx, 0, sunPitchR),
        toothedGear(placed(sc.sun.get), sunStroke, 0.55, 0.6),
        {
          val sunHubR = sunRole match
            case Role.Plain => 2.4
            case _          => 3.4
          val sunHubW = sunRole match
            case Role.Plain => 0.8
            case _          => 2.0
          s.circle(s.cx := f"$cx%.2f", s.cy := "0", s.r := f"$sunHubR%.2f",
                   s.fill := HubFill, s.stroke := sunStroke,
                   s.strokeWidth := f"$sunHubW%.2f")
        }
      )

    // Ring-level static IN/OUT badge near the ring rim (placed outside the
    // rotating ring group so the badge text stays upright).
    val ringRoleBadge = ringRole match
      case Role.Plain => s.g()
      case _          => roleBadge(ringRole, lay.ringBadge._1, lay.ringBadge._2)

    // Sun-level static IN/OUT badge at the centre (offset slightly above the
    // sun pin so the rotating sun doesn't sweep over it).
    val sunRoleBadge = sunRole match
      case Role.Plain         => s.g()
      case _ if !showSun      => s.g()
      case _                  => roleBadge(sunRole, lay.sunBadge._1, lay.sunBadge._2)

    // Carrier: outer <g> rotates about stage center; planets sit inside at static
    // angles, with their own inner <g> handling per-planet spin in the carrier frame.
    val carrierStroke = Palette.css(sc.carrier.tone)
    val carrierStrokeW = roleStrokeWidth(carrierRole, 1.2)
    val carrierOpacity = if carrierRole == Role.Plain && carrierSlot.isEmpty then "0.7" else "1.0"
    val planetStroke   = Palette.css(sc.planets.head.tone)
    val carrierG = s.g(
      svg.transform <-- carrierSig.map(rotateXform(_, cx, 0)),
      // Coupled carrier: a solid band under the dashed circle, so it shows even when a role colour is drawn.
      carrierSlot.fold(s.g())(sl =>
        s.circle(s.cx := f"$cx%.2f", s.cy := "0", s.r := f"$carrierR%.2f", s.fill := "none",
                 s.stroke := CouplingColors(sl), s.strokeWidth := "4.5", s.strokeOpacity := "0.45")),
      // Carrier circle (rotationally symmetric, but inside this group anyway)
      s.circle(
        s.cx := f"$cx%.2f", s.cy := "0",
        s.r := f"$carrierR%.2f",
        s.fill := "none", s.stroke := carrierStroke,
        s.strokeWidth := f"$carrierStrokeW%.2f",
        svg.strokeDashArray := "3 3", svg.strokeLineCap := "butt",
        s.strokeOpacity := carrierOpacity
      ),
      sc.planets.flatMap { pg =>
        val (pxC, pyC) = SchematicLayout.gearCentre(pg, k, cx)
        // Carrier spoke (static within carrier <g>)
        val spoke = s.line(
          s.x1 := f"$cx%.2f", s.y1 := "0",
          s.x2 := f"$pxC%.2f", s.y2 := f"$pyC%.2f",
          s.stroke := carrierStroke, s.strokeOpacity := (if carrierSlot.isDefined then "0.8" else "0.35"),
          s.strokeWidth := (if carrierSlot.isDefined then "2.2" else "1.2")
        )
        // Planet body: spin relative to the carrier frame
        val planet = s.g(
          svg.transform <-- angles.map(a => rotateXform(GearboxScene.angle(pg, a), pxC, pyC)),
          pitchCircle(pxC, pyC, planetPitchR),
          toothedGear(placed(pg), planetStroke, 0.55, 0.6),
          s.circle(s.cx := f"$pxC%.2f", s.cy := f"$pyC%.2f", s.r := "1.6",
                   s.fill := HubFill, s.stroke := planetStroke, s.strokeWidth := "0.6")
        )
        // "Same part" marker at the pin of a stacked planet; counter-rotated so the glyph stays upright.
        val pinMark = planetSlot.fold(s.g())(sl =>
          s.g(
            svg.transform <-- carrierSig.map(c => rotateXform(-c, pxC, pyC)),
            s.pointerEvents := "none",
            s.circle(s.cx := f"$pxC%.2f", s.cy := f"$pyC%.2f", s.r := "5.2",
                     s.fill := HubFill, s.stroke := CouplingColors(sl), s.strokeWidth := "1.4"),
            s.text(s.x := f"$pxC%.2f", s.y := f"${pyC + 3.6}%.2f", s.textAnchor := "middle",
                   s.fill := CouplingColors(sl), s.fontSize := "11", s.fontWeight := "700",
                   s.fontFamily := "ui-sans-serif, system-ui", "≡")
          ))
        Vector(spoke, planet, pinMark)
      }
    )

    // Carrier-level IN badge at the bottom of the carrier circle.
    val carrierRoleBadge = carrierRole match
      case Role.Plain => s.g()
      case _          => roleBadge(carrierRole, lay.carrierBadge._1, lay.carrierBadge._2)

    val labelText =
      val pr = s"P${sc.planets.head.teeth} R${sc.ring.teeth}"
      sc.sun.fold(s"$label · $pr")(g => s"$label · S${g.teeth} $pr")
    val labelEl = s.text(
      s.x := f"$cx%.2f", s.y := f"${lay.labelY}%.2f", s.textAnchor := "middle",
      s.fill := SubText, s.fontSize := "11", s.fontFamily := "ui-monospace, monospace",
      labelText
    )

    // Coupling bands just outside the ring rim and the sun's tips; hub ring for a coupled carrier.
    val bands = Vector(
      band(ringSlot, ringOR + 2.5),
      if showSun then band(sunSlot, lay.sunOR + 2.0) else s.g(),
      carrierSlot.fold(s.g())(sl =>
        s.circle(s.cx := f"$cx%.2f", s.cy := "0", s.r := "6.5", s.fill := "none",
                 s.stroke := CouplingColors(sl), s.strokeWidth := "2", s.pointerEvents := "none"))
    )

    Vector(ringTint, ringEl, carrierG, sunEl) ++ bands ++ Vector(ringRoleBadge, sunRoleBadge, carrierRoleBadge, labelEl)
