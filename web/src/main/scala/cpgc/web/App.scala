package cpgc.web

import com.raquo.laminar.api.L.*
import cpgc.constraints.{SearchConfig, Topology}
import cpgc.domain.CompoundDesign
import cpgc.search.Scorer.SortBy
import cpgc.web.UiKit.*
import com.raquo.laminar.nodes.ChildNode
import org.scalajs.dom
import scala.scalajs.js

object App:

  def main(args: Array[String]): Unit =
    documentEvents(_.onDomContentLoaded).foreach { _ =>
      val container = dom.document.getElementById("app")
      render(container, root())
    }(unsafeWindowOwner)

  private def query: String = dom.window.location.search

  // ── Hard bounds for sliders (absolute min/max the user can drag to). ──────
  private val SunRange     = (2, 100)
  private val PlanetRange  = (2, 60)
  private val RingRange    = (10, 200)
  private val PlanetsRange = (2, 8)

  /** Mirror the page state into the address bar so the URL round-trips. */
  private def writeUrl(f: FormValues, sort: SortBy, view: View): Unit =
    try
      val extra = if query.contains("pause") then "&pause" else ""
      dom.window.history.replaceState(null, "", "?" + UrlState.toQuery(f, sort, view) + extra)
    catch case _: Throwable => () // e.g. sandboxed file:// pages

  private def root(): HtmlElement =
    val animator = Animator()
    animator.start()
    if urlHas("pause") then animator.running.set(false)
    val form0         = UrlState.initialForm(query)
    val topologyVar   = Var[Topology](form0.topology)
    val sunMin        = Var(form0.sunMin)
    val sunMax        = Var(form0.sunMax)
    val planetMin     = Var(form0.planetMin)
    val planetMax     = Var(form0.planetMax)
    val ringMin       = Var(form0.ringMin)
    val ringMax       = Var(form0.ringMax)
    val planetsMin    = Var(form0.planetsMin)
    val planetsMax    = Var(form0.planetsMax)
    val odVar         = Var(form0.od)
    val minModule     = Var(form0.minModule)
    val sunPlanetFact = Var(form0.sunPlanetFactor)
    val targetVar     = Var(form0.target)
    val phasedVar     = Var(form0.phasedPinions)
    val ringWallVar   = Var(form0.ringWall)
    val clearanceVar  = Var(form0.clearance)
    val sortByVar     = Var[SortBy](UrlState.initialSort(query))
    val viewVar       = Var[View](UrlState.initialView(query))

    val resultsVar     = Var(Vector.empty[CompoundDesign])
    val selectedVar    = Var(Option.empty[CompoundDesign])
    val statusVar      = Var("")
    val errorVar       = Var(Option.empty[String])
    val searchState    = Var[SearchState](SearchState.Searching)
    val searchProgress = Var(0)
    val searchProgressFraction = Var(0.0)
    val advancedConstraintsOpen: Var[Boolean] = Var(false)
    def formNow(): FormValues = FormValues(
      topology = topologyVar.now(),
      sunMin = sunMin.now(), sunMax = sunMax.now(),
      planetMin = planetMin.now(), planetMax = planetMax.now(),
      ringMin = ringMin.now(), ringMax = ringMax.now(),
      planetsMin = planetsMin.now(), planetsMax = planetsMax.now(),
      od = odVar.now(), minModule = minModule.now(),
      sunPlanetFactor = sunPlanetFact.now(), target = targetVar.now(),
      phasedPinions = phasedVar.now(), ringWall = ringWallVar.now(), clearance = clearanceVar.now()
    )

    val runner = SearchRunner(
      () => formNow(), sortByVar, resultsVar, selectedVar, statusVar, errorVar,
      searchState, searchProgress, searchProgressFraction, (f, sort) => writeUrl(f, sort, viewVar.now())
    )
    viewVar.signal.changes.foreach(v => writeUrl(formNow(), sortByVar.now(), v))(unsafeWindowOwner)

    js.timers.setTimeout(50)(runner.runSearch())

    // Live search: any input change triggers a debounced re-search.
    val inputStreams: Seq[EventStream[Unit]] = Seq[Signal[Any]](
      topologyVar.signal,
      sunMin.signal, sunMax.signal,
      planetMin.signal, planetMax.signal,
      ringMin.signal, ringMax.signal,
      planetsMin.signal, planetsMax.signal,
      odVar.signal, minModule.signal,
      sunPlanetFact.signal, targetVar.signal,
      phasedVar.signal, ringWallVar.signal, clearanceVar.signal
    ).map(_.changes.map(_ => ()))
    EventStream.merge(inputStreams*)
      .debounce(200)
      .foreach(_ => runner.runSearch())(unsafeWindowOwner)

    val hasTargets: Signal[Boolean] = targetVar.signal.map(t => UiLogic.parseTargets(t).nonEmpty)

    div(
      cls := "app",
      // ── Toast stack — transient notifications, top-right ─────────────
      div(cls := "toast-stack",
        children <-- toasts.signal.map { ts =>
          ts.map { t =>
            div(
              cls := "toast",
              cls := s"toast-${t.level.toString.toLowerCase}",
              span(cls := "toast-msg", t.message),
              button(
                cls := "toast-dismiss",
                ariaLabel := "Dismiss",
                "✕",
                onClick.preventDefault --> { _ => dismissToast(t.id) }
              )
            )
          }
        }
      ),
      div(cls := "top-grid",
        // ── Left: form ─────────────────────────────────────────────────────
        div(cls := "panel form-panel",
          h2("Constraints"),
          TopologyPanel(topologyVar, phasedVar),
          labeled("Target ratio(s)",
            tooltip = "Reduction ratios you want. Separate several with ';' or a space (e.g. 100; 53.12). " +
              "A comma between digits is a decimal comma (53,12 = 53.12). Leave blank to list all designs."
          ) { id =>
            input(idAttr := id, typ := "text", placeholder := "e.g. 53.12 or blank for all",
              controlled(value <-- targetVar.signal, onInput.mapToValue --> targetVar))
          },
          div(cls := "field-hint",
            child.text <-- targetVar.signal.map(t => UiLogic.targetHint(t).getOrElse(""))),
          rangeRow("Sun teeth",    sunMin,     sunMax,     SunRange,
            tooltip = "Range of sun-gear tooth counts to search (not used by a sun-less stage)."),
          rangeRow("Planet teeth", planetMin,  planetMax,  PlanetRange,
            tooltip = "Range of planet-gear tooth counts to search."),
          rangeRow("Ring teeth",   ringMin,    ringMax,    RingRange,
            tooltip = "Range of ring-gear tooth counts to search."),
          rangeRow("# planets",    planetsMin, planetsMax, PlanetsRange,
            tooltip = "Number of planet gears arranged around the sun. More planets = smoother load sharing; 3–4 is most common."),
          labeled("First-stage OD (mm)",
            tooltip = "Outer diameter of stage 1's ring gear in millimetres, wall included. Sets the overall scale of the gearbox.")(
            id => numInput(odVar, id)),
          // ── Advanced gear constraints ──────────────────────────────────
          disclosure("Advanced gear constraints", advancedConstraintsOpen, emptyMod,
            labeled("Min module (mm/tooth)",
              tooltip = "Smallest acceptable gear-tooth size. 'Module' is mm of pitch diameter per tooth — a manufacturability floor.")(
              id => numInput(minModule, id)),
            labeled("Max planet/sun ratio",
              tooltip = "Reject designs where the planet has more teeth than this multiple of the sun's teeth. Blank uses the default (3).")(
              id => input(idAttr := id, typ := "text", inputMode := "decimal", placeholder := SearchConfig.DefaultSunPlanetFactor.toString,
                controlled(value <-- sunPlanetFact.signal, onInput.mapToValue --> sunPlanetFact))),
            labeled("Ring wall (mm)",
              tooltip = "Material between the ring's tooth roots and its outside diameter. The OD bounds the ring's outside.")(
              id => numInput(ringWallVar, id)),
            labeled("Planet clearance (modules)",
              tooltip = "Minimum gap between the tips of adjacent planets, in modules.")(
              id => numInput(clearanceVar, id))
          ),
          div(cls := "form-error", roleAttr := "alert",
            child.text <-- errorVar.signal.map(_.getOrElse("")))
        ),
        // ── Right: schematic ─────────────────────────────────────────
        div(cls := "panel schematic-panel",
          div(cls := "panel-header",
            span(cls := "panel-title", "Schematic"),
            ThreeView.toggle(viewVar),
            div(cls := "controls",
              label(cls := "speed-label", forId := "speed-input", "speed"),
              input(
                idAttr := "speed-input",
                cls := "speed",
                typ := "range",
                minAttr := "0", maxAttr := "3", stepAttr := "0.05",
                controlled(
                  value <-- animator.speed.signal.map(_.toString),
                  onInput.mapToValue.map(s => UiLogic.parseFinite(s).getOrElse(animator.speed.now())) --> animator.speed
                )
              ),
              span(cls := "rpm-readout",
                child.text <-- animator.speed.signal.map(s => f"${s * 60}%.0f rpm (input)")),
              button(
                cls := "iconbtn",
                ariaLabel := "Pause or resume the animation",
                child.text <-- animator.running.signal.map(if _ then "⏸" else "▶"),
                onClick.preventDefault --> { _ => animator.running.update(!_) }
              )
            )
          ),
          div(cls := "schematic-sub",
            child.text <-- selectedVar.signal.map {
              case Some(d) => Results.detailLine(d)
              case None    => ""
            }
          ),
          div(cls := "schematic",
            child <-- viewVar.signal.map[ChildNode.Base] {
              case View.Schematic =>
                Schematic(selectedVar.signal, animator.phase, searchState.signal, searchProgress.signal, searchProgressFraction.signal)
              case View.ThreeD =>
                ThreeView(selectedVar.signal, animator.phase, searchState.signal, searchProgress.signal)
            })
        )
      ),
      // ── Bottom: results table + export bar ──────────────────────────
      Results.panel(resultsVar, selectedVar, sortByVar, hasTargets, runner.chooseSort,
        statusVar, searchState, searchProgress)
    )

  private def urlHas(name: String): Boolean =
    query.stripPrefix("?").split('&').exists(p => p == name || p.startsWith(name + "="))

  /** Min/max range row: label, dual-knob slider (clipped to slider bounds for
    * display only), and two number inputs that accept ANY integer (no
    * clamping). If `lo > hi` at search time, `buildConfig` swaps them. */
  private def rangeRow(
      name: String, lo: Var[Int], hi: Var[Int], bounds: (Int, Int), tooltip: String = ""
  ): HtmlElement =
    val (absMin, absMax) = bounds
    inline def clipToSlider(v: Int): Int = math.max(absMin, math.min(v, absMax))
    val loId = freshId("range")
    val hiId = freshId("range")
    div(cls := "range-row",
      label(forId := loId, titleAttr := tooltip, name),
      div(cls := "range-slider",
        div(cls := "range-track"),
        div(cls := "range-track-fill",
          styleAttr <-- lo.signal.combineWith(hi.signal).map { (l, h) =>
            val span      = (absMax - absMin).toDouble.max(1.0)
            val loClip    = math.max(absMin, math.min(math.min(l, h), absMax))
            val hiClip    = math.max(absMin, math.min(math.max(l, h), absMax))
            val loFrac    = (loClip - absMin) / span
            val widthFrac = (hiClip - loClip) / span
            f"left: calc((100%% - 14px) * $loFrac%.4f + 7px); width: calc((100%% - 14px) * $widthFrac%.4f);"
          }
        ),
        input(
          cls := "range-low", typ := "range", ariaLabel := s"$name minimum (slider)",
          minAttr := absMin.toString, maxAttr := absMax.toString, stepAttr := "1",
          controlled(
            value <-- lo.signal.map(v => clipToSlider(v).toString),
            onInput.mapToValue.map(_.toIntOption.getOrElse(lo.now())) --> lo
          )
        ),
        input(
          cls := "range-high", typ := "range", ariaLabel := s"$name maximum (slider)",
          minAttr := absMin.toString, maxAttr := absMax.toString, stepAttr := "1",
          controlled(
            value <-- hi.signal.map(v => clipToSlider(v).toString),
            onInput.mapToValue.map(_.toIntOption.getOrElse(hi.now())) --> hi
          )
        )
      ),
      div(cls := "range-inputs",
        intInput(lo, loId, s"$name minimum"),
        intInput(hi, hiId, s"$name maximum")
      )
    )

  /** Label + control; `mk` receives the id that ties them together. */
  private def labeled(name: String, tooltip: String = "")(mk: String => HtmlElement): HtmlElement =
    val id = freshId("field")
    div(
      if tooltip.nonEmpty then label(forId := id, titleAttr := tooltip, name) else label(forId := id, name),
      mk(id)
    )

  /** Text input bound to an Int that keeps what the user typed and commits
    * only text that parses (so "-" or "" never overwrite the value). */
  private def intInput(v: Var[Int], id: String, aria: String): HtmlElement =
    val text = Var(v.now().toString)
    input(
      idAttr := id, typ := "text", inputMode := "numeric", ariaLabel := aria,
      cls("invalid") <-- text.signal.map(t => t.trim.toIntOption.isEmpty),
      value := text.now(),
      value <-- v.signal.changes.filter(x => UiLogic.shouldRewrite(text.now(), x)).map { x => text.set(x.toString); x.toString },
      onInput.mapToValue --> Observer[String] { s =>
        text.set(s)
        s.trim.toIntOption.foreach(v.set)
      }
    )

  private def fmtField(d: Double): String =
    if d == math.rint(d) && math.abs(d) < 1e15 then d.toLong.toString else d.toString

  /** Text input bound to a Double; same keep-the-text rule as `intInput`. */
  private def numInput(v: Var[Double], id: String): HtmlElement =
    val text = Var(fmtField(v.now()))
    input(
      idAttr := id, typ := "text", inputMode := "decimal",
      cls("invalid") <-- text.signal.map(t => UiLogic.parseFinite(t).isEmpty),
      ariaInvalid <-- text.signal.map(t => UiLogic.parseFinite(t).isEmpty.toString),
      value := text.now(),
      value <-- v.signal.changes.filter(x => UiLogic.shouldRewrite(text.now(), x)).map { x => val s = fmtField(x); text.set(s); s },
      onInput.mapToValue --> Observer[String] { s =>
        text.set(s)
        UiLogic.parseFinite(s).foreach(v.set)
      }
    )
