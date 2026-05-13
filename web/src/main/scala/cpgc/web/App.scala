package cpgc.web

import com.raquo.laminar.api.L.*
import cpgc.Calculator
import cpgc.constraints.{SearchConfig, TeethBounds}
import cpgc.domain.{CompoundDesign, Mode, ProfileShift, Ratio}
import cpgc.`export`.{Csv, Json, OpenScad}
import org.scalajs.dom
import scala.scalajs.js

object App:

  def main(args: Array[String]): Unit =
    documentEvents(_.onDomContentLoaded).foreach { _ =>
      val container = dom.document.getElementById("app")
      render(container, root())
    }(unsafeWindowOwner)

  private def parseDouble(s: String): Option[Double] =
    s.trim.replace(',', '.').toDoubleOption

  private def initialMode(): Mode =
    val q = dom.window.location.search
    if q.contains("mode=single") then Mode.SingleStage
    else if q.contains("mode=cdcp") then Mode.CDCP
    else if q.contains("mode=nss")  then Mode.NSS
    else Mode.Standard

  // ── Hard bounds for sliders (absolute min/max the user can drag to). ──────
  private val SunRange     = (2, 100)
  private val PlanetRange  = (2, 60)
  private val RingRange    = (10, 200)
  private val PlanetsRange = (2, 8)

  private def root(): HtmlElement =
    val animator = Animator()
    animator.start()
    if dom.window.location.search.contains("pause") then animator.running.set(false)
    val modeVar       = Var[Mode](initialMode())
    val sunMin        = Var(10)
    val sunMax        = Var(40)
    val planetMin     = Var(10)
    val planetMax     = Var(18)
    val ringMin       = Var(40)
    val ringMax       = Var(120)
    val planetsMin    = Var(3)
    val planetsMax    = Var(4)
    val odVar         = Var(150.0)
    val minModule     = Var(0.5)
    val sunPlanetFact = Var("")    // empty = unlimited
    val targetVar     = Var("53.12")
    val topK          = Var(20)

    val resultsVar  = Var(Vector.empty[CompoundDesign])
    val selectedVar = Var(Option.empty[CompoundDesign])
    val statusVar   = Var("")

    // Normalise an inverted (lo > hi) pair before passing it to TeethBounds, so
    // the user can drag/type freely without tripping the `max >= min` invariant.
    def bounds(lo: Var[Int], hi: Var[Int]): TeethBounds =
      val l = math.min(lo.now(), hi.now())
      val h = math.max(lo.now(), hi.now())
      TeethBounds(l, h)

    def runSearch(): Unit =
      val t0 = js.Date.now()
      val cfg = SearchConfig(
        mode            = modeVar.now(),
        sunTeeth        = bounds(sunMin, sunMax),
        planetTeeth     = bounds(planetMin, planetMax),
        ringTeeth       = bounds(ringMin, ringMax),
        planets         = bounds(planetsMin, planetsMax),
        firstStageOD    = odVar.now(),
        minModule       = minModule.now(),
        sunPlanetFactor = parseDouble(sunPlanetFact.now()).getOrElse(1e9),
        topPerTarget    = topK.now(),
        targets         = targetVar.now().split(',').toList.flatMap(s => parseDouble(s)).map(Ratio.apply)
      )
      val designs =
        if cfg.targets.isEmpty then Calculator.search(cfg).take(topK.now()).toVector
        else
          val best = Calculator.best(cfg)
          cfg.targets.flatMap(t => best.getOrElse(t, Vector.empty).map(_.design)).toVector
      val elapsed = js.Date.now() - t0
      resultsVar.set(designs)
      selectedVar.set(designs.headOption)
      statusVar.set(s"${designs.size} designs · ${elapsed.toInt} ms")

    dom.window.setTimeout(() => runSearch(), 50)

    div(
      cls := "app",
      div(cls := "top-grid",
        // ── Left: form ───────────────────────────────────────────────
        div(cls := "panel form-panel",
          h2("Constraints"),
          labeled("Mode")(modeSelect(modeVar)),
          rangeRow("Sun teeth",    sunMin,     sunMax,     SunRange),
          rangeRow("Planet teeth", planetMin,  planetMax,  PlanetRange),
          rangeRow("Ring teeth",   ringMin,    ringMax,    RingRange),
          rangeRow("# planets",    planetsMin, planetsMax, PlanetsRange),
          labeled("First-stage OD (mm)")(numInput(odVar)),
          labeled("Min module (mm/tooth)")(numInput(minModule)),
          labeled("Max planet/sun ratio (blank = unlimited)")(
            input(
              typ := "text", inputMode := "decimal",
              placeholder := "unlimited",
              controlled(value <-- sunPlanetFact.signal, onInput.mapToValue --> sunPlanetFact)
            )
          ),
          labeled("Target ratios (comma-separated, blank for enumeration)")(strInput(targetVar)),
          labeled("Top K")(intInput(topK)),
          button(
            cls := "go",
            "Search",
            onClick.preventDefault --> { _ => runSearch() }
          ),
          div(cls := "status", child.text <-- statusVar.signal)
        ),
        // ── Right: schematic ─────────────────────────────────────────
        div(cls := "panel schematic-panel",
          div(cls := "panel-header",
            span(cls := "panel-title", "Schematic"),
            div(cls := "controls",
              button(
                cls := "iconbtn",
                child.text <-- animator.running.signal.map(if _ then "⏸" else "▶"),
                onClick.preventDefault --> { _ => animator.running.update(!_) }
              ),
              label(cls := "speed-label", "speed"),
              input(
                cls := "speed",
                typ := "range",
                minAttr := "0", maxAttr := "3", stepAttr := "0.05",
                controlled(
                  value <-- animator.speed.signal.map(_.toString),
                  onInput.mapToValue.map(s => parseDouble(s).getOrElse(animator.speed.now())) --> animator.speed
                )
              ),
              span(cls := "panel-sub",
                child.text <-- animator.speed.signal.map(s => f"${s * 60}%.0f rpm in"))
            )
          ),
          div(cls := "schematic-sub",
            child.text <-- selectedVar.signal.map {
              case Some(d) => detailLine(d)
              case None    => ""
            }
          ),
          div(cls := "schematic", Schematic(selectedVar.signal, animator.phase))
        )
      ),
      // ── Bottom: results table + export bar ──────────────────────────
      div(cls := "panel results-panel",
        div(cls := "panel-header",
          span(cls := "panel-title", "Results"),
          div(cls := "exports",
            exportButton("CSV",  "text/csv", "csv",
              () => Csv.render(resultsVar.now()), resultsVar.signal.map(_.isEmpty)),
            exportButton("JSON", "application/json", "json",
              () => Json.render(resultsVar.now()), resultsVar.signal.map(_.isEmpty)),
            scadButton(selectedVar)
          )
        ),
        resultsTable(resultsVar.signal, selectedVar)
      )
    )

  // ── Export helpers ─────────────────────────────────────────────────────────

  private def exportButton(
      label: String, mime: String, ext: String,
      content: () => String, isDisabled: Signal[Boolean]
  ): HtmlElement =
    button(
      cls := "export-btn",
      label,
      disabled <-- isDisabled,
      onClick.preventDefault --> { _ =>
        val stamp = js.Date.now().toLong
        download(s"cpgc-$stamp.$ext", mime, content())
      }
    )

  private def scadButton(selected: Var[Option[CompoundDesign]]): HtmlElement =
    button(
      cls := "export-btn",
      "SCAD",
      disabled <-- selected.signal.map(_.isEmpty),
      onClick.preventDefault --> { _ =>
        selected.now().foreach { d =>
          val ratio = math.abs(d.ratio.value).round
          download(s"cpgc-design-$ratio.scad", "application/x-openscad", OpenScad.emit(d))
        }
      }
    )

  private def download(filename: String, mime: String, content: String): Unit =
    val options = js.Dynamic.literal(`type` = mime).asInstanceOf[dom.BlobPropertyBag]
    val parts   = js.Array[dom.BlobPart](content)
    val blob    = new dom.Blob(parts, options)
    val url     = dom.URL.createObjectURL(blob)
    val a       = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
    a.href = url
    a.setAttribute("download", filename)
    a.click()
    dom.URL.revokeObjectURL(url)

  // ── Detail / table ─────────────────────────────────────────────────────────

  private def detailLine(d: CompoundDesign): String =
    val s1 = d.stage1
    val base = s"Ratio ${fmt3(d.ratio.value)}:1   η ${fmt3(d.efficiency)}   N=${s1.set.numPlanets}"
    val mods = d.stage2 match
      case Some(b) => s"   m₁=${fmt3(s1.module.value)} m₂=${fmt3(b.module.value)}"
      case None    => s"   m=${fmt3(s1.module.value)}"
    val shift = d.profileShiftHint match
      case Some(ProfileShift(x1, x2, alpha)) =>
        f"   shift x₁=$x1%.3f x₂=$x2%.3f α'=$alpha%.2f°"
      case None => ""
    base + mods + shift

  private def fmt3(d: Double): String =
    val rounded = math.round(d * 1000).toDouble / 1000.0
    rounded.toString

  private def resultsTable(
      designs: Signal[Vector[CompoundDesign]],
      selected: Var[Option[CompoundDesign]]
  ): HtmlElement =
    div(cls := "table-wrap",
      table(
        thead(tr(
          th("ratio"), th("eff"), th("N"), th("spacing"),
          th("S₁"), th("P₁"), th("R₁"), th("m₁"),
          th("S₂"), th("P₂"), th("R₂"), th("m₂")
        )),
        tbody(children <-- designs.map { ds =>
          ds.map { d =>
            tr(
              cls := "row",
              cls("sel") <-- selected.signal.map(_.contains(d)),
              onClick --> { _ => selected.set(Some(d)) },
              td(cls := "num", fmt3(d.ratio.value)),
              td(cls := "num", fmt3(d.efficiency)),
              td(d.stage1.set.numPlanets.toString),
              td(if d.spacingOk then "ok" else "—"),
              td(d.stage1.set.sun.toInt.toString),
              td(d.stage1.set.planet.toInt.toString),
              td(d.stage1.set.ring.toInt.toString),
              td(cls := "num", fmt3(d.stage1.module.value)),
              td(d.stage2.fold("-")(_.set.sun.toInt.toString)),
              td(d.stage2.fold("-")(_.set.planet.toInt.toString)),
              td(d.stage2.fold("-")(_.set.ring.toInt.toString)),
              td(cls := "num", d.stage2.fold("-")(s => fmt3(s.module.value)))
            )
          }
        })
      )
    )

  // ── Inputs ─────────────────────────────────────────────────────────────────

  private def modeSelect(v: Var[Mode]): HtmlElement =
    select(
      onChange.mapToValue.map(s => Mode.parse(s).getOrElse(Mode.Standard)) --> v,
      Mode.values.toSeq.map(m => option(value := m.toString, m.toString, selected <-- v.signal.map(_ == m)))
    )

  /** Min/max range row: label, dual-knob slider (clipped to slider bounds for
    * display only), and two number inputs that accept ANY integer (no
    * clamping). The slider's `min`/`max` attributes limit the drag range, but
    * the user can always type outside that range in the number boxes. If
    * `lo > hi` at search time, `runSearch` silently swaps them before
    * constructing `TeethBounds`.
    */
  private def rangeRow(
      name: String, lo: Var[Int], hi: Var[Int], bounds: (Int, Int)
  ): HtmlElement =
    val (absMin, absMax) = bounds

    inline def clipToSlider(v: Int): Int = math.max(absMin, math.min(v, absMax))

    div(cls := "range-row",
      label(name),
      div(cls := "range-slider",
        div(cls := "range-track"),
        div(cls := "range-track-fill",
          styleAttr <-- lo.signal.combineWith(hi.signal).map { (l, h) =>
            val span  = (absMax - absMin).toDouble.max(1.0)
            val left  = 100.0 * (math.min(l, h) - absMin).max(0).toDouble.min(span) / span
            val width = 100.0 * math.abs(h - l).toDouble.min(span) / span
            f"left: $left%.2f%%; width: $width%.2f%%"
          }
        ),
        input(
          cls := "range-low", typ := "range",
          minAttr := absMin.toString, maxAttr := absMax.toString, stepAttr := "1",
          controlled(
            // Display value clipped to slider range; underlying Var may hold a
            // value outside that range (when the user typed it).
            value <-- lo.signal.map(v => clipToSlider(v).toString),
            onInput.mapToValue.map(_.toIntOption.getOrElse(lo.now())) --> lo
          )
        ),
        input(
          cls := "range-high", typ := "range",
          minAttr := absMin.toString, maxAttr := absMax.toString, stepAttr := "1",
          controlled(
            value <-- hi.signal.map(v => clipToSlider(v).toString),
            onInput.mapToValue.map(_.toIntOption.getOrElse(hi.now())) --> hi
          )
        )
      ),
      div(cls := "range-inputs",
        intInput(lo),
        intInput(hi)
      )
    )

  private def labeled(name: String)(el: HtmlElement): HtmlElement = div(label(name), el)

  private def intInput(v: Var[Int]): HtmlElement =
    input(
      typ := "number",
      controlled(
        value <-- v.signal.map(_.toString),
        onInput.mapToValue.map(_.toIntOption.getOrElse(v.now())) --> v
      )
    )

  private def numInput(v: Var[Double]): HtmlElement =
    input(
      typ := "text",
      inputMode := "decimal",
      controlled(
        value <-- v.signal.map(_.toString),
        onInput.mapToValue.map(s => parseDouble(s).getOrElse(v.now())) --> v
      )
    )

  private def strInput(v: Var[String]): HtmlElement =
    input(
      typ := "text",
      controlled(
        value <-- v.signal,
        onInput.mapToValue --> v
      )
    )
