package cpgc.web

import com.raquo.laminar.api.L.*
import cpgc.Calculator
import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, Topology}
import cpgc.domain.{CompoundDesign, Validity}
import cpgc.`export`.{Csv, Json, OpenScad}
import cpgc.search.Scorer
import cpgc.search.Scorer.SortBy
import org.scalajs.dom
import scala.scalajs.js

/** UX-level state of the schematic preview. Drives which placeholder text is
  * shown when there is no `selectedDesign` to render.
  *
  *  - `Searching` — a search is in flight; show a transient placeholder.
  *  - `NoResults` — the most recent search completed and returned 0 designs.
  *  - `HasResults` — the most recent search produced ≥ 1 design.
  *  - `Invalid` — the form does not describe a search; the reason is shown inline.
  */
enum SearchState:
  case Searching, NoResults, HasResults, Invalid

/** Severity of a transient toast notification. */
enum ToastLevel:
  case Info, Warn, Error

/** One transient toast message. `id` is a monotonic sequence used for keyed
  * rendering and dismissal. */
final case class Toast(id: Long, message: String, level: ToastLevel)

/** What the last finished search produced. `designs` is the complete result
  * set when `rerankable` (so a sort change needs no new search). */
final case class FinishedRun(cfg: SearchConfig, designs: Vector[CompoundDesign], rerankable: Boolean)

object App:

  /** Laminar 17 doesn't expose the HTML `title` attribute under a non-conflicting
    * name (the symbol `title` is the `<title>` element). Bind it explicitly. */
  private val titleAttr: HtmlAttr[String] =
    htmlAttr("title", com.raquo.laminar.codecs.StringAsIsCodec)
  private val ariaExpanded: HtmlAttr[String] = htmlAttr("aria-expanded", com.raquo.laminar.codecs.StringAsIsCodec)
  private val ariaPressed: HtmlAttr[String]  = htmlAttr("aria-pressed", com.raquo.laminar.codecs.StringAsIsCodec)
  private val ariaLabel: HtmlAttr[String]    = htmlAttr("aria-label", com.raquo.laminar.codecs.StringAsIsCodec)
  private val ariaInvalid: HtmlAttr[String]  = htmlAttr("aria-invalid", com.raquo.laminar.codecs.StringAsIsCodec)
  private val roleAttr: HtmlAttr[String]     = htmlAttr("role", com.raquo.laminar.codecs.StringAsIsCodec)

  // ── Toast notifications ────────────────────────────────────────────────────
  // A dependency-free transient-message stack for anything that would otherwise
  // die silently in the browser console.

  private val toasts: Var[Vector[Toast]] = Var(Vector.empty)
  private var toastIdSeq: Long = 0L

  /** Append a toast; auto-dismiss after 5 s. */
  private def notify(message: String, level: ToastLevel = ToastLevel.Error): Unit =
    toastIdSeq += 1
    val id = toastIdSeq
    toasts.update(_ :+ Toast(id, message, level))
    js.timers.setTimeout(5000)(toasts.update(_.filterNot(_.id == id)))

  private def dismissToast(id: Long): Unit =
    toasts.update(_.filterNot(_.id == id))

  private var idSeq = 0
  private def freshId(prefix: String): String =
    idSeq += 1
    s"$prefix-$idSeq"

  // ── Gearbox-type presets ──────────────────────────────────────────────────

  /** Short user-facing label for the picker. */
  private def presetLabel(p: GearboxPreset): String = p match
    case GearboxPreset.SingleStage       => "Single-stage planetary"
    case GearboxPreset.SunDriven         => "Sun-driven compound (Standard)"
    case GearboxPreset.CarrierDriven     => "Carrier-driven compound (CDCP)"
    case GearboxPreset.SunlessCdcp       => "CDCP (sun-less)"
    case GearboxPreset.NoSecondSun       => "No-second-sun compound (NSS)"
    case GearboxPreset.RingToSunCascade  => "Ring-to-sun cascade"
    case GearboxPreset.Ravigneaux        => "Ravigneaux"
    case GearboxPreset.Simpson           => "Simpson"
    case GearboxPreset.Custom            => "Custom (edited below)"

  /** One-line plain-English description shown under the picker. */
  private def presetDescription(p: GearboxPreset): String = p match
    case GearboxPreset.SingleStage       => "One planetary stage. Sun in, carrier out, ring grounded."
    case GearboxPreset.SunDriven         => "Classic stacked-planet compound. Sun in, ring-2 out."
    case GearboxPreset.CarrierDriven     => "Same compound, carrier as input. Smoother low-ratio reductions."
    case GearboxPreset.SunlessCdcp       => "Carrier in, no sun gears at all: the planets mesh only with the two rings (the reference program's CDCP)."
    case GearboxPreset.NoSecondSun       => "Stage-2 has no sun (one fewer mesh → higher efficiency)."
    case GearboxPreset.RingToSunCascade  => "Stage-1 ring drives stage-2 sun. Series-like compound."
    case GearboxPreset.Ravigneaux        => "Stage-1 carrier drives stage-2 sun; rings welded. Automotive AT."
    case GearboxPreset.Simpson           => "Stage-1 carrier drives stage-2 ring; suns welded. Automotive AT."
    case GearboxPreset.Custom            => "Custom topology: adjust the suns, couplings and roles below."

  /** Largest result set the engine keeps. A run that found no more than this
    * has the complete set and can be re-ranked without searching again (ADR 0010). */
  private val TopK = 16_000

  /** Search units run between clock checks inside a chunk. */
  private val StepUnits = 256

  /** How long a single search chunk runs before yielding the JS thread. */
  private val ChunkBudgetMs = 12

  /** Hard wall-clock cap on a single search. After this the engine is stopped
    * and its best-so-far is reported as truncated. */
  private val MaxSearchMs = 5_000

  /** Minimum time the schematic stays in the `Searching` state, so the
    * progress bar does not flash on fast searches. */
  private val MinSearchingMs = 500

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
  private def writeUrl(f: FormValues, sort: SortBy): Unit =
    try
      val extra = if query.contains("pause") then "&pause" else ""
      dom.window.history.replaceState(null, "", "?" + UiLogic.toQuery(f, sort) + extra)
    catch case _: Throwable => () // e.g. sandboxed file:// pages

  private def root(): HtmlElement =
    val animator = Animator()
    animator.start()
    if urlHas("pause") then animator.running.set(false)
    val form0         = UiLogic.initialForm(query)
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
    val displayCount  = Var(10)
    val sortByVar     = Var[SortBy](UiLogic.initialSort(query))

    val resultsVar     = Var(Vector.empty[CompoundDesign])
    val selectedVar    = Var(Option.empty[CompoundDesign])
    val statusVar      = Var("")
    val errorVar       = Var(Option.empty[String])
    val searchState    = Var[SearchState](SearchState.Searching)
    val searchProgress = Var(0)
    val searchProgressFraction = Var(0.0)
    val advancedConstraintsOpen: Var[Boolean] = Var(false)
    var lastRun = Option.empty[FinishedRun]
    // Cancellation token: each search bumps the epoch; in-flight chunks of an
    // older search exit on their next iteration.
    var searchEpoch = 0L

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

    def clearResults(): Unit =
      lastRun = None
      resultsVar.set(Vector.empty)
      selectedVar.set(None)

    def runSearch(): Unit =
      searchEpoch += 1
      val epoch = searchEpoch
      val form  = formNow()
      writeUrl(form, sortByVar.now())
      clearResults()
      statusVar.set("")
      searchProgress.set(0)
      searchProgressFraction.set(0.0)
      UiLogic.buildConfig(form, TopK) match
        case Left(msg) =>
          errorVar.set(Some(msg))
          searchState.set(SearchState.Invalid)
        case Right(cfg) =>
          errorVar.set(None)
          searchState.set(SearchState.Searching)
          try startSearch(epoch, cfg)
          catch
            case e: Throwable =>
              errorVar.set(Some(s"Search failed: ${e.getMessage}"))
              searchState.set(SearchState.Invalid)

    def startSearch(epoch: Long, cfg: SearchConfig): Unit =
      val t0     = js.Date.now()
      val sortBy = sortByVar.now()
      val engine = Calculator.engine(cfg, sortBy, TopK)

      def chunk(): Unit =
        if epoch != searchEpoch then return
        val deadline = js.Date.now() + ChunkBudgetMs
        var done     = engine.done
        try
          while !done && js.Date.now() < deadline do done = engine.step(StepUnits)
        catch
          case e: Throwable =>
            errorVar.set(Some(s"Search failed: ${e.getMessage}"))
            searchState.set(SearchState.Invalid)
            return
        searchProgress.set(engine.found.toInt)
        val totalElapsed = js.Date.now() - t0
        searchProgressFraction.set(math.min(1.0, totalElapsed / MaxSearchMs.toDouble))
        val expired = totalElapsed >= MaxSearchMs
        if !done && !expired then js.timers.setTimeout(0)(chunk())
        else
          if !done then engine.stop()
          def finish(): Unit =
            if epoch == searchEpoch then
              val designs   = engine.top
              val elapsed   = (js.Date.now() - t0).toInt
              val rerank    = UiLogic.rerankable(engine.done, engine.truncated, engine.found, TopK)
              lastRun = Some(FinishedRun(cfg, designs, rerank))
              resultsVar.set(designs)
              selectedVar.set(designs.headOption)
              statusVar.set(
                if engine.truncated then s"$elapsed ms · best so far — truncated, narrow your ranges"
                else if engine.found > TopK then s"$elapsed ms · top $TopK of ${engine.found}"
                else s"$elapsed ms"
              )
              searchState.set(if designs.nonEmpty then SearchState.HasResults else SearchState.NoResults)
          val holdMs = MinSearchingMs - totalElapsed
          if holdMs <= 0 then finish()
          else
            searchProgressFraction.set(1.0)
            js.timers.setTimeout(holdMs)(finish())

      // Defer the first chunk so the placeholder paints first.
      js.timers.setTimeout(0)(chunk())

    /** Sort-pill click: re-rank the finished set in place when it is
      * complete, otherwise search again with the new ranking. */
    def chooseSort(by: SortBy): Unit =
      if by != sortByVar.now() then
        sortByVar.set(by)
        lastRun match
          case Some(run) if run.rerankable =>
            writeUrl(formNow(), by)
            val ranked = Scorer.sort(run.designs, by, run.cfg.targets)
            resultsVar.set(ranked)
            selectedVar.update(sel => sel.filter(ranked.contains).orElse(ranked.headOption))
          case _ => runSearch()

    js.timers.setTimeout(50)(runSearch())

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
      .foreach(_ => runSearch())(unsafeWindowOwner)

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
          topologyControls(topologyVar, phasedVar),
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
              case Some(d) => detailLine(d)
              case None    => ""
            }
          ),
          div(cls := "schematic", Schematic(selectedVar.signal, animator.phase, searchState.signal, searchProgress.signal, searchProgressFraction.signal))
        )
      ),
      // ── Bottom: results table + export bar ──────────────────────────
      div(cls := "panel results-panel",
        div(cls := "panel-header",
          span(cls := "panel-title", "Results"),
          div(cls := "sort-pills", roleAttr := "group", ariaLabel := "Sort results by",
            List(SortBy.BestFit, SortBy.Ratio, SortBy.Efficiency, SortBy.Simplicity).map(by =>
              sortPill(sortByVar, by, hasTargets, chooseSort))
          ),
          div(cls := "exports",
            exportButton("CSV",  "text/csv", "csv",
              () => Csv.render(resultsVar.now()), resultsVar.signal.map(_.isEmpty)),
            exportButton("JSON", "application/json", "json",
              () => Json.render(resultsVar.now()), resultsVar.signal.map(_.isEmpty)),
            scadButton(selectedVar)
          )
        ),
        resultsTable(resultsVar.signal, selectedVar, displayCount.signal),
        div(cls := "show-more-row",
          span(cls := "panel-sub", roleAttr := "status",
            child.text <-- resultsVar.signal
              .combineWith(displayCount.signal, statusVar.signal, searchState.signal, searchProgress.signal)
              .map { (ds, n, status, state, progress) =>
                state match
                  case SearchState.Searching => s"Searching… $progress found"
                  case SearchState.Invalid   => "Fix the input error to search."
                  case _ =>
                    val shown = math.min(n, ds.size)
                    val countText = s"Showing $shown of ${ds.size}"
                    if status.isEmpty then countText else s"$countText  ·  $status"
              }
          ),
          div(cls := "show-more-right",
            label(cls := "rows-label", forId := "rows-select", "Rows"),
            select(cls := "rows-select", idAttr := "rows-select",
              onChange.mapToValue --> Observer[String] { v =>
                if v == "all" then displayCount.set(resultsVar.now().size.max(1))
                else v.toIntOption.foreach(n => displayCount.set(n.max(1)))
              },
              List(10, 20, 50, 100, 200).map { n =>
                option(value := n.toString,
                       selected <-- displayCount.signal.map(_ == n),
                       n.toString)
              },
              option(value := "all",
                     selected <-- displayCount.signal.combineWith(resultsVar.signal)
                                    .map { (n, ds) => ds.nonEmpty && n >= ds.size && !Set(10,20,50,100,200).contains(n) },
                     "All")
            )
          )
        )
      )
    )

  private def urlHas(name: String): Boolean =
    query.stripPrefix("?").split('&').exists(p => p == name || p.startsWith(name + "="))

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

  /** Save `content` as a file. The anchor is attached to the document for the
    * click (Firefox), and the object URL is revoked after the download started. */
  private def download(filename: String, mime: String, content: String): Unit =
    val options = js.Dynamic.literal(`type` = mime).asInstanceOf[dom.BlobPropertyBag]
    val parts   = js.Array[dom.BlobPart](content)
    val blob    = new dom.Blob(parts, options)
    val url     = dom.URL.createObjectURL(blob)
    val a       = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
    a.href = url
    a.setAttribute("download", filename)
    a.style.display = "none"
    dom.document.body.appendChild(a)
    a.click()
    js.timers.setTimeout(1000) {
      dom.document.body.removeChild(a)
      dom.URL.revokeObjectURL(url)
    }

  // ── Detail / table ─────────────────────────────────────────────────────────

  private def detailLine(d: CompoundDesign): String =
    val s1 = d.stage1
    val base = s"Ratio ${fmtRatio(d.ratio.value)}:1   η ${UiLogic.fmtEfficiency(d.efficiency)}   N=${UiLogic.planetCountLabel(d)}"
    val mods = d.stage2 match
      case Some(b) => s"   m₁=${fmt3(s1.module.value)} m₂=${fmt3(b.module.value)}"
      case None    => s"   m=${fmt3(s1.module.value)}"
    val warn = d.validity match
      case Validity.Warning(reason) => s"   ⚠ $reason"
      case Validity.Ok              => ""
    base + mods + warn

  private def fmt3(d: Double): String =
    val rounded = math.round(d * 1000).toDouble / 1000.0
    rounded.toString

  /** Display a ratio with explicit sign. Negative ratios (output rotates
    * opposite to input) print with a leading "−" and an arrow glyph. */
  private def fmtRatio(r: Double): String =
    val mag = fmt3(math.abs(r))
    if r < 0 then s"−$mag ↺" else mag

  private def resultsTable(
      designs: Signal[Vector[CompoundDesign]],
      selected: Var[Option[CompoundDesign]],
      displayCount: Signal[Int]
  ): HtmlElement =
    div(cls := "table-wrap",
      table(
        thead(tr(
          th(cls := "col-warn", titleAttr := "Warnings about this design (assemblability hints, etc.). Empty when the design has no warnings.", ""),
          th(titleAttr := "Overall reduction ratio (input speed ÷ output speed).", "ratio"),
          th(titleAttr := "Estimated mechanical efficiency, from the count and types of meshes.", "eff"),
          th(titleAttr := "Number of planets in stage 1.", "N"),
          th(titleAttr := "Stage 1 sun gear tooth count (— when stage 1 has no sun).", "S₁"),
          th(titleAttr := "Stage 1 planet gear tooth count.", "P₁"),
          th(titleAttr := "Stage 1 ring gear tooth count.", "R₁"),
          th(titleAttr := "Stage 1 module (mm per tooth).", "m₁"),
          th(titleAttr := "Stage 2 sun gear tooth count (— when stage 2 has no sun).", "S₂"),
          th(titleAttr := "Stage 2 planet gear tooth count.", "P₂"),
          th(titleAttr := "Stage 2 ring gear tooth count.", "R₂"),
          th(titleAttr := "Stage 2 module (mm per tooth).", "m₂")
        )),
        tbody(children <-- designs.combineWith(displayCount).map { (ds, n) =>
          ds.take(n).map { d =>
            tr(
              cls := "row",
              tabIndex := 0,
              cls("sel") <-- selected.signal.map(_.contains(d)),
              onClick --> { _ => selected.set(Some(d)) },
              onKeyDown.filter(e => e.key == "Enter" || e.key == " ").preventDefault --> { _ => selected.set(Some(d)) },
              td(cls := "col-warn",
                d.validity match
                  case Validity.Warning(reason) =>
                    span(cls := "warn-glyph", titleAttr := reason, "⚠")
                  case Validity.Ok => span()
              ),
              td(cls := "num", fmtRatio(d.ratio.value)),
              td(cls := "num", UiLogic.fmtEfficiency(d.efficiency)),
              td(UiLogic.planetCountLabel(d)),
              td(if d.topology.s1Present then d.stage1.set.sun.toInt.toString else "—"),
              td(d.stage1.set.planet.toInt.toString),
              td(d.stage1.set.ring.toInt.toString),
              td(cls := "num", fmt3(d.stage1.module.value)),
              td(d.stage2.fold("—") { s => if d.topology.s2Present then s.set.sun.toInt.toString else "—" }),
              td(d.stage2.fold("—")(_.set.planet.toInt.toString)),
              td(d.stage2.fold("—")(_.set.ring.toInt.toString)),
              td(cls := "num", d.stage2.fold("—")(s => fmt3(s.module.value)))
            )
          }
        })
      )
    )

  // ── Disclosure ─────────────────────────────────────────────────────────────

  /** A collapsible section: a `<button aria-expanded>` header over its content. */
  private def disclosure(
      title: String, open: Var[Boolean], suffix: Modifier[HtmlElement], content: Modifier[HtmlElement]*
  ): HtmlElement =
    val contentId = freshId("disc")
    div(cls := "advanced-section",
      button(
        cls := "advanced-summary", typ := "button",
        ariaExpanded <-- open.signal.map(_.toString),
        aria.controls := contentId,
        onClick.preventDefault --> { _ => open.update(!_) },
        span(cls := "advanced-arrow", child.text <-- open.signal.map(if _ then "▾" else "▸")),
        s" $title",
        suffix
      ),
      div(idAttr := contentId, cls := "advanced-content",
        cls("hidden") <-- open.signal.map(!_),
        content
      )
    )

  // ── Topology controls ──────────────────────────────────────────────────────

  /** A `<select>` whose DOM value is forced back to `current()` after each
    * change, so a rejected edit never leaves the wrong option displayed. */
  private def pickSelect(current: () => String)(handle: String => Unit)(mods: Modifier[HtmlElement]*): HtmlElement =
    select(
      mods,
      inContext { (el: HtmlElement) =>
        onChange --> Observer[dom.Event] { _ =>
          val sel = el.ref.asInstanceOf[dom.html.Select]
          handle(sel.value)
          sel.value = current()
        }
      }
    )

  private def topologyControls(v: Var[Topology], phased: Var[Boolean]): HtmlElement =
    val stagesSig    = v.signal.map(_.stages)
    val compoundSig  = v.signal.map(t => !UiLogic.couplingSelectorsDisabled(t))
    val inputBody    = v.signal.map(_.input)
    val outputBody   = v.signal.map(_.output)
    val fixedBody    = v.signal.map(_.fixed)
    val presetSig    = v.signal.map(UiLogic.topologyPreset)
    val stackedSig   = v.signal.map(_.stacked)
    val advancedOpen: Var[Boolean] = Var(false)
    // A derived-Custom topology opens the section so the edits are visible.
    presetSig.foreach { p => if p == GearboxPreset.Custom then advancedOpen.set(true) }(unsafeWindowOwner)

    def sunToggle(stage: Int, text: String): HtmlElement =
      val present = v.signal.map(t => t.roles.get(if stage == 1 then Body.S1 else Body.S2).exists(_ != BodyRole.Absent))
      val reason  = v.signal.map(UiLogic.sunToggleDisabledReason(_, stage))
      div(
        label(cls := "topology-toggle",
          cls("disabled") <-- reason.map(_.isDefined),
          titleAttr <-- reason.map(_.getOrElse("")),
          input(typ := "checkbox",
            disabled <-- reason.map(_.isDefined),
            controlled(
              checked <-- present,
              onInput.mapToChecked --> Observer[Boolean] { b =>
                UiLogic.setSunPresent(v.now(), stage, b) match
                  case Right(t)  => v.set(t)
                  case Left(msg) => notify(s"Cannot change the sun: $msg", ToastLevel.Warn)
              }
            )
          ),
          span(cls := "switch"),
          span(text)
        ),
        div(cls := "field-hint", child.text <-- reason.map(_.fold("")(r => s"Locked: $r")))
      )

    def couplingPicker(slot: CouplingSlot, name: String, tip: String, pick: Topology => Coupling): Modifier[HtmlElement] =
      val sid = freshId("coupling")
      Seq(
        label(cls := "stage-coupling-row", forId := sid, cls("disabled") <-- compoundSig.map(!_), name),
        pickSelect(() => pick(v.now()).toString)(n => v.update(t => UiLogic.setCoupling(t, slot, Coupling.valueOf(n))))(
          idAttr := sid, cls := "role-select",
          disabled <-- compoundSig.map(!_),
          titleAttr := tip,
          Coupling.values.toSeq.map { c =>
            option(
              value := c.toString,
              selected <-- v.signal.map(pick(_) == c),
              disabled <-- v.signal.map(UiLogic.couplingOptionDisabled(_, slot, c)),
              titleAttr <-- v.signal.map(UiLogic.couplingOptionReason(_, slot, c).getOrElse("")),
              Coupling.label(c)
            )
          }
        )
      )

    def rolePicker(name: String, currentSig: Signal[Body], current: Topology => Body, role: BodyRole, tip: String): Modifier[HtmlElement] =
      val rid = freshId("role")
      Seq(
        label(forId := rid, name),
        pickSelect(() => current(v.now()).toString)(n => v.update(UiLogic.assignRole(_, Body.valueOf(n), role)))(
          idAttr := rid, cls := "role-select", titleAttr := tip,
          // Bodies that do not exist in this topology (absent suns, stage 2 of a single stage) are greyed out.
          Body.values.toSeq.map { b =>
            option(
              value := b.toString,
              selected <-- currentSig.map(_ == b),
              disabled <-- v.signal.map(t => !t.roles.contains(b) || t.roles.get(b).contains(BodyRole.Absent)),
              Body.label(b)
            )
          }
        )
      )

    div(cls := "topology",
      // ── Gearbox-type preset picker — the primary topology control ─────────
      label(forId := "preset-select", "Gearbox type"),
      pickSelect(() => UiLogic.topologyPreset(v.now()).toString) { name =>
        val p = GearboxPreset.valueOf(name)
        if p != GearboxPreset.Custom then v.set(UiLogic.presetTopology(p))
      }(
        idAttr := "preset-select", cls := "preset-select",
        titleAttr := "Pick a named gearbox family. Most users don't need to touch the Advanced section.",
        UiLogic.selectablePresets.map { p =>
          option(value := p.toString, selected <-- presetSig.map(_ == p), presetLabel(p))
        },
        // Shown only as the derived state of an edited topology; not a choice.
        option(value := GearboxPreset.Custom.toString, disabled := true, hidden <-- presetSig.map(_ != GearboxPreset.Custom),
               selected <-- presetSig.map(_ == GearboxPreset.Custom), presetLabel(GearboxPreset.Custom))
      ),
      div(cls := "topology-derived", child.text <-- presetSig.map(presetDescription)),
      // ── Advanced section — suns, couplings, roles ─────────────────────────
      disclosure("Advanced topology", advancedOpen,
        span(cls := "advanced-customized",
          child.text <-- presetSig.map(p => if p == GearboxPreset.Custom then "  (customized)" else "")),
        label("Stages"),
        div(cls := "topology-segment", roleAttr := "group", ariaLabel := "Number of stages",
          button(
            typ := "button",
            cls("active") <-- stagesSig.map(_ == 1),
            ariaPressed <-- stagesSig.map(s => (s == 1).toString),
            titleAttr := "One planetary stage: sun + planets + ring + carrier.",
            "Single stage",
            onClick.preventDefault --> { _ => v.update(UiLogic.setStageCount(_, 1)) }
          ),
          button(
            typ := "button",
            cls("active") <-- stagesSig.map(_ == 2),
            ariaPressed <-- stagesSig.map(s => (s == 2).toString),
            titleAttr := "Two planetary stages; compound reduction.",
            "Compound (2 stages)",
            onClick.preventDefault --> { _ => v.update(UiLogic.setStageCount(_, 2)) }
          )
        ),
        sunToggle(1, "Stage 1 has its own sun gear"),
        sunToggle(2, "Stage 2 has its own sun gear"),
        div(cls := "field-hint",
          "Without a sun the planets mesh only with the ring, so the sun-teeth range and its spacing rule do not apply to that stage."),
        couplingPicker(CouplingSlot.First, "Coupling 1",
          "First inter-stage weld. The classic compound uses Carrier 1 ↔ Carrier 2.", _.firstCoupling),
        couplingPicker(CouplingSlot.Second, "Coupling 2",
          "Second inter-stage weld. The classic compound uses stacked planets (P1 ↔ P2).", _.secondCoupling),
        div(cls := "coupling-note",
          child.text <-- v.signal.map { t =>
            if t.stages != 2 then ""
            else
              Seq(Coupling.manufacturingNote(t.firstCoupling), Coupling.manufacturingNote(t.secondCoupling))
                .filter(_.nonEmpty).mkString("  ·  ")
          }
        ),
        // Phased pinions: only meaningful for stacked planets.
        div(
          cls("hidden") <-- stackedSig.map(!_),
          label(cls := "topology-toggle",
            input(typ := "checkbox",
              controlled(checked <-- phased.signal, onInput.mapToChecked --> phased)),
            span(cls := "switch"),
            span("Individually phased pinions")
          ),
          div(cls := "field-hint",
            "Off: every planet is the same one-piece part. On: each planet gets its own twist between the two halves, " +
            "so more tooth combinations assemble, but the N planets are different parts.")
        ),
        rolePicker("Input", inputBody, _.input, BodyRole.Input, "Body driven externally (e.g. by a motor)."),
        rolePicker("Output", outputBody, _.output, BodyRole.Output, "Body whose speed defines the gearbox ratio."),
        rolePicker("Fixed", fixedBody, _.fixed, BodyRole.Fixed, "Body grounded to the housing (ω = 0)."),
        div(cls := "topology-derived legacy-derived",
          child.text <-- v.signal.map(t => s"→ ${topologyLabel(t)}"))
      )
    )

  private def topologyLabel(t: Topology): String =
    val legacy = t.legacyMode.map(_.toString).getOrElse(if t == Topology.sunlessCdcpCompound then "CDCP sun-less" else "custom")
    val suns =
      Seq(Option.when(!t.s1Present)("no sun 1"), Option.when(t.stages == 2 && !t.s2Present)("no sun 2")).flatten
    val sunText = if suns.isEmpty then "" else suns.mkString(", ", ", ", "")
    val pins = s"${Body.label(t.input).toLowerCase} in, ${Body.label(t.output).toLowerCase} out, ${Body.label(t.fixed).toLowerCase} fixed"
    val kind = if t.stages == 1 then "single-stage" else "compound"
    s"$kind: $pins$sunText ($legacy)"

  private def sortPill(
      v: Var[SortBy], target: SortBy, hasTargets: Signal[Boolean], choose: SortBy => Unit
  ): HtmlElement =
    button(
      cls := "sort-pill", typ := "button",
      cls("active") <-- v.signal.map(_ == target),
      ariaPressed <-- v.signal.map(s => (s == target).toString),
      titleAttr <-- hasTargets.map(UiLogic.sortTooltip(target, _)),
      child.text <-- hasTargets.map(UiLogic.sortLabel(target, _)),
      onClick.preventDefault --> { _ => choose(target) }
    )

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
