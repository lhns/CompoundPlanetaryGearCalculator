package cpgc.web

import com.raquo.laminar.api.L.*
import cpgc.Calculator
import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.domain.{CompoundDesign, Mode, ProfileShift, Ratio, Validity}
import cpgc.`export`.{Csv, Json, OpenScad}
import cpgc.search.Scorer
import cpgc.search.Scorer.SortBy
import org.scalajs.dom
import scala.scalajs.js

/** UX-level state of the schematic preview. Drives which placeholder text is
  * shown when there is no `selectedDesign` to render.
  *
  *  - `Searching` — a `runSearch` invocation is in flight; show a transient
  *    "Searching designs…" placeholder.
  *  - `NoResults` — the most recent search completed and returned 0 designs;
  *    show the actionable "no design matches" message.
  *  - `HasResults` — the most recent search produced ≥ 1 design; the schematic
  *    renders that design, so this state never produces a placeholder.
  */
enum SearchState:
  case Searching, NoResults, HasResults

/** Severity of a transient toast notification. */
enum ToastLevel:
  case Info, Warn, Error

/** One transient toast message. `id` is a monotonic sequence used for keyed
  * rendering and dismissal. */
final case class Toast(id: Long, message: String, level: ToastLevel)

/** Named gearbox families. Each maps to a complete `Topology` (stages, couplings,
  * roles); the UI picker shows these instead of the raw coupling math for the
  * common case. `Custom` is the catch-all for topologies that don't match any
  * named family. */
enum GearboxPreset:
  case SingleStage
  case SunDriven       // Standard compound: stacked planet, sun in, R2 out
  case CarrierDriven   // CDCP: stacked planet, carrier in, R2 out
  case NoSecondSun     // NSS: stacked planet, no S2
  case RingToSunCascade
  case Ravigneaux
  case Simpson
  case Custom

object App:

  /** Laminar 17 doesn't expose the HTML `title` attribute under a non-conflicting
    * name (the symbol `title` is the `<title>` element). Bind it explicitly. */
  private val titleAttr: HtmlAttr[String] =
    htmlAttr("title", com.raquo.laminar.codecs.StringAsIsCodec)

  // ── Toast notifications ────────────────────────────────────────────────────
  // A dependency-free transient-message stack. The app is a single-instance
  // SPA, so a module-level `Var` is the simplest home. `notify` is the
  // last-resort surface for anything that would otherwise die silently in the
  // browser console.

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

  /** Set a `Var` from a value whose construction may throw an
    * `IllegalArgumentException` (a `Topology` invariant). On failure the `Var`
    * is left untouched — never poisoned — and the reason is surfaced as a
    * toast. This is the structural guard against the `VarError: Unable to
    * update a failed Var` cascade. */
  private def safeSet[A](v: Var[A], next: => A): Unit =
    try v.set(next)
    catch
      case e: IllegalArgumentException =>
        notify(s"Invalid configuration: ${e.getMessage}", ToastLevel.Error)

  // ── Gearbox-type presets ──────────────────────────────────────────────────
  // The constraints panel offers a "Gearbox type" picker as its primary
  // topology control. Each preset is a complete `Topology` (stages, couplings,
  // roles). For topologies that don't match any named preset, the picker
  // shows `Custom` and the Advanced section is the way to edit them.

  private def presetTopology(p: GearboxPreset): Topology = UiLogic.presetTopology(p)

  private def topologyPreset(t: Topology): GearboxPreset = UiLogic.topologyPreset(t)

  /** Short user-facing label for the picker. */
  private def presetLabel(p: GearboxPreset): String = p match
    case GearboxPreset.SingleStage       => "Single-stage planetary"
    case GearboxPreset.SunDriven         => "Sun-driven compound (Standard)"
    case GearboxPreset.CarrierDriven     => "Carrier-driven compound (CDCP)"
    case GearboxPreset.NoSecondSun       => "No-second-sun compound (NSS)"
    case GearboxPreset.RingToSunCascade  => "Ring-to-sun cascade"
    case GearboxPreset.Ravigneaux        => "Ravigneaux"
    case GearboxPreset.Simpson           => "Simpson"
    case GearboxPreset.Custom            => "Custom…"

  /** One-line plain-English description shown under the picker. */
  private def presetDescription(p: GearboxPreset): String = p match
    case GearboxPreset.SingleStage       => "One planetary stage. Sun in, carrier out, ring grounded."
    case GearboxPreset.SunDriven         => "Classic stacked-planet compound. Sun in, ring-2 out."
    case GearboxPreset.CarrierDriven     => "Same compound, carrier as input. Smoother low-ratio reductions."
    case GearboxPreset.NoSecondSun       => "Stage-2 has no sun (one fewer mesh → higher efficiency)."
    case GearboxPreset.RingToSunCascade  => "Stage-1 ring drives stage-2 sun. Series-like compound."
    case GearboxPreset.Ravigneaux        => "Stage-1 carrier drives stage-2 sun; rings welded. Automotive AT."
    case GearboxPreset.Simpson           => "Stage-1 carrier drives stage-2 ring; suns welded. Automotive AT."
    case GearboxPreset.Custom            => "Custom topology — adjust couplings & roles below."

  /** Safety ceiling on result-set size — never binds for sensible tooth-count
    * bounds (typical search produces O(100)–O(1000) designs); prevents
    * pathological-bounds searches from freezing the tab. See ADR-0010.
    */
  private val ComputeCap = 10_000

  /** How long a single search chunk runs before yielding the JS thread.
    * Tuned a touch under one 60 fps frame so the browser has slack to paint
    * and process events while a heavy search is in flight — especially
    * useful in Firefox, whose JS engine processes our linear-algebra
    * `LinAlg.solve` hot loop slower than V8 does. */
  private val ChunkBudgetMs = 12

  /** Hard wall-clock cap on a single search. After this, chunking stops and
    * whatever has been collected is reported as a truncated result set. Keeps
    * pathological searches (very wide tooth-count ranges) from triggering
    * the browser's "Page Unresponsive" dialog. */
  private val MaxSearchMs = 5_000

  /** Minimum time the schematic stays in the `Searching` state, even if the
    * search itself finishes sooner. Without this, default-config searches
    * complete in ~50 ms and the progress bar flashes too briefly to register
    * visually. 500 ms is comfortably under the perceptual "slow" threshold
    * while long enough that the bar + count are legible.
    */
  private val MinSearchingMs = 500

  def main(args: Array[String]): Unit =
    documentEvents(_.onDomContentLoaded).foreach { _ =>
      val container = dom.document.getElementById("app")
      render(container, root())
    }(unsafeWindowOwner)

  private def parseDouble(s: String): Option[Double] = UiLogic.parseDouble(s)

  private def query: String = dom.window.location.search

  // ── Hard bounds for sliders (absolute min/max the user can drag to). ──────
  private val SunRange     = (2, 100)
  private val PlanetRange  = (2, 60)
  private val RingRange    = (10, 200)
  private val PlanetsRange = (2, 8)

  private def root(): HtmlElement =
    val animator = Animator()
    animator.start()
    if query.contains("pause") then animator.running.set(false)
    // Test-only: force a no-results state for screenshot verification.
    val forceNoResults = query.contains("nodesigns")
    val form0         = UiLogic.initialForm(query)
    val topologyVar   = Var[Topology](form0.topology)
    // URL-driven initial values (see `UiLogic.initialForm`).
    val sunMin        = Var(form0.sunMin)
    val sunMax        = Var(form0.sunMax)
    val planetMin     = Var(form0.planetMin)
    val planetMax     = Var(form0.planetMax)
    val ringMin       = Var(if forceNoResults then 200 else form0.ringMin)
    val ringMax       = Var(if forceNoResults then 200 else form0.ringMax)
    val planetsMin    = Var(form0.planetsMin)
    val planetsMax    = Var(form0.planetsMax)
    val odVar         = Var(form0.od)
    val minModule     = Var(form0.minModule)
    val sunPlanetFact = Var(form0.sunPlanetFactor)
    val targetVar     = Var(form0.target)
    val displayCount  = Var(10)
    val sortByVar     = Var[SortBy](SortBy.BestFit)

    val resultsVar     = Var(Vector.empty[CompoundDesign])
    val selectedVar    = Var(Option.empty[CompoundDesign])
    val statusVar      = Var("")
    val searchState    = Var[SearchState](SearchState.Searching)
    // Live count of designs accumulated by the in-flight search. Updated at
    // the end of every chunk; reset to 0 when a new search begins. Drives the
    // `Searching designs… (N found)` placeholder + the in-flight status line.
    val searchProgress = Var(0)
    // Elapsed-wall-clock fraction in [0, 1] of MaxSearchMs. Drives the
    // schematic's in-flight progress bar fill width. Reaches 1.0 when the
    // wall-clock cap triggers.
    val searchProgressFraction = Var(0.0)
    // Open-state for the "Advanced gear constraints" disclosure (min module +
    // max planet/sun ratio — technical knobs most users never touch).
    val advancedConstraintsOpen: Var[Boolean] = Var(false)

    // Normalise an inverted (lo > hi) pair before passing it to TeethBounds, so
    // the user can drag/type freely without tripping the `max >= min` invariant.
    // Cancellation token for the chunked search. Each `runSearch` call bumps
    // the epoch; in-flight chunks that captured an older epoch exit on their
    // next iteration, freeing the JS thread for the new search.
    var searchEpoch = 0L

    def runSearch(): Unit =
      searchEpoch += 1
      val epoch = searchEpoch
      searchState.set(SearchState.Searching)
      searchProgress.set(0)
      searchProgressFraction.set(0.0)
      // Clear the previous design so the schematic shows the searching
      // placeholder rather than a stale design rendered under a freshly-
      // changed topology (which would be a design/topology desync).
      selectedVar.set(None)
      val t0 = js.Date.now()
      val cfg = UiLogic.buildConfig(
        FormValues(
          topology = topologyVar.now(),
          sunMin = sunMin.now(), sunMax = sunMax.now(),
          planetMin = planetMin.now(), planetMax = planetMax.now(),
          ringMin = ringMin.now(), ringMax = ringMax.now(),
          planetsMin = planetsMin.now(), planetsMax = planetsMax.now(),
          od = odVar.now(), minModule = minModule.now(),
          sunPlanetFactor = sunPlanetFact.now(), target = targetVar.now()
        ),
        topPerTarget = ComputeCap
      )

      // The phased state machine lives in `Calculator.searchSteps`. Each
      // `steps.next()` does one unit of work; this loop is purely scheduling.
      val acc   = scala.collection.mutable.ArrayBuffer.empty[CompoundDesign]
      val steps = Calculator.searchSteps(cfg)

      def chunk(): Unit =
        if epoch != searchEpoch then return
        val deadline = js.Date.now() + ChunkBudgetMs
        try
          while steps.hasNext && acc.size < ComputeCap && js.Date.now() < deadline do
            steps.next().foreach(acc.+=)
        catch
          case e: Throwable =>
            // The search core is meant to be total; if anything still throws,
            // surface it as a toast instead of dying silently in the console.
            notify(s"Search failed: ${e.getMessage}", ToastLevel.Error)
            searchState.set(SearchState.NoResults)
            return
        searchProgress.set(acc.size)
        val totalElapsed = js.Date.now() - t0
        searchProgressFraction.set(math.min(1.0, totalElapsed / MaxSearchMs.toDouble))
        val done    = !steps.hasNext
        val capped  = acc.size >= ComputeCap
        val expired = totalElapsed >= MaxSearchMs
        if !done && !capped && !expired then
          js.timers.setTimeout(0)(chunk())
        else
          val truncated = !done && expired
          def finalize(): Unit =
            if epoch != searchEpoch then ()
            else
              val designs = Scorer.sort(acc.toVector, sortByVar.now(), cfg.targets).take(ComputeCap)
              val elapsed = js.Date.now() - t0
              resultsVar.set(designs)
              selectedVar.set(designs.headOption)
              displayCount.set(10)
              statusVar.set(
                if truncated then
                  s"${elapsed.toInt} ms (truncated after $MaxSearchMs ms — narrow your ranges)"
                else
                  s"${elapsed.toInt} ms"
              )
              searchState.set(
                if designs.nonEmpty then SearchState.HasResults else SearchState.NoResults
              )
          // Hold the bar visible for at least MinSearchingMs so the user
          // actually sees it on fast searches. During the hold, pin the fill
          // to 100% (the shimmer keeps animating via CSS).
          val holdMs = MinSearchingMs - totalElapsed
          if holdMs <= 0 then finalize()
          else
            searchProgressFraction.set(1.0)
            js.timers.setTimeout(holdMs)(finalize())

      // Defer the first chunk so the "Searching designs…" placeholder paints
      // before we start consuming the iterator.
      js.timers.setTimeout(0)(chunk())

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
      sortByVar.signal
    ).map(_.changes.map(_ => ()))
    EventStream.merge(inputStreams*)
      .debounce(200)
      .foreach(_ => runSearch())(unsafeWindowOwner)

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
                "✕",
                onClick.preventDefault --> { _ => dismissToast(t.id) }
              )
            )
          }
        }
      ),
      div(cls := "top-grid",
        // ── Left: form ─────────────────────────────────────────────────────
        // Task-flow order: pick the gearbox type, say what reduction you
        // want, set the gear-tooth search space, set the physical size, with
        // technical knobs hidden behind disclosures.
        div(cls := "panel form-panel",
          h2("Constraints"),
          topologyControls(topologyVar),
          labeled("Target ratio(s)",
            tooltip = "Reduction ratios you want, comma-separated (e.g. 100, 53.12). Leave blank to enumerate all designs in the search space.",
            placeholderText = Some("e.g. 53.12 or blank for all")
          )(strInput(targetVar)),
          rangeRow("Sun teeth",    sunMin,     sunMax,     SunRange,
            tooltip = "Range of sun-gear tooth counts to search."),
          rangeRow("Planet teeth", planetMin,  planetMax,  PlanetRange,
            tooltip = "Range of planet-gear tooth counts to search."),
          rangeRow("Ring teeth",   ringMin,    ringMax,    RingRange,
            tooltip = "Range of ring-gear tooth counts to search (derived as r = s + 2p)."),
          rangeRow("# planets",    planetsMin, planetsMax, PlanetsRange,
            tooltip = "Number of planet gears arranged around the sun. More planets = smoother load sharing; 3–4 is most common."),
          labeled("First-stage OD (mm)",
            tooltip = "Approximate outer diameter of stage 1's ring gear, in millimetres. Sets the overall scale of the gearbox.")(numInput(odVar)),
          // ── Advanced gear constraints ──────────────────────────────────
          div(cls := "advanced-section",
            div(cls := "advanced-summary",
              onClick.preventDefault --> { _ => advancedConstraintsOpen.update(!_) },
              span(cls := "advanced-arrow",
                child.text <-- advancedConstraintsOpen.signal.map(if _ then "▾" else "▸")),
              " Advanced gear constraints"
            ),
            div(cls := "advanced-content",
              cls("hidden") <-- advancedConstraintsOpen.signal.map(!_),
              labeled("Min module (mm/tooth)",
                tooltip = "Smallest acceptable gear-tooth size. 'Module' is mm of pitch diameter per tooth — a manufacturability floor.")(numInput(minModule)),
              labeled("Max planet/sun ratio",
                tooltip = "Reject designs where the planet has more teeth than this multiple of the sun's teeth. Limits how oversized planets can be relative to the sun.")(
                input(
                  typ := "text", inputMode := "decimal",
                  placeholder := "unlimited",
                  controlled(value <-- sunPlanetFact.signal, onInput.mapToValue --> sunPlanetFact)
                )
              )
            )
          )
        ),
        // ── Right: schematic ─────────────────────────────────────────
        div(cls := "panel schematic-panel",
          div(cls := "panel-header",
            span(cls := "panel-title", "Schematic"),
            div(cls := "controls",
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
              span(cls := "rpm-readout",
                child.text <-- animator.speed.signal.map(s => f"${s * 60}%.0f rpm (input)")),
              button(
                cls := "iconbtn",
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
          div(cls := "schematic", Schematic(selectedVar.signal, animator.phase, topologyVar.signal, searchState.signal, searchProgress.signal, searchProgressFraction.signal))
        )
      ),
      // ── Bottom: results table + export bar ──────────────────────────
      div(cls := "panel results-panel",
        div(cls := "panel-header",
          span(cls := "panel-title", "Results"),
          div(cls := "sort-pills",
            sortPill(sortByVar, SortBy.BestFit,    "Best fit",
              "Closest match to your target ratio. Pick this when you typed a target."),
            sortPill(sortByVar, SortBy.Ratio,      "Ratio",
              "Smallest reduction ratio first. Pick this to browse from low to high reduction."),
            sortPill(sortByVar, SortBy.Efficiency, "Efficiency",
              "Most mechanically efficient designs first. Pick this to minimise transmission losses."),
            sortPill(sortByVar, SortBy.Simplicity, "Simplicity",
              "Fewest total teeth first. Pick this to favour compact, printable, cheaper designs.")
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
          span(cls := "panel-sub",
            child.text <-- resultsVar.signal
              .combineWith(displayCount.signal, statusVar.signal, searchState.signal, searchProgress.signal)
              .map { (ds, n, status, state, progress) =>
                if state == SearchState.Searching then
                  s"Searching… $progress found"
                else
                  val shown = math.min(n, ds.size)
                  val countText = s"Showing $shown of ${ds.size}"
                  if status.isEmpty then countText else s"$countText  ·  $status"
              }
          ),
          div(cls := "show-more-right",
            label(cls := "rows-label", "Rows"),
            select(cls := "rows-select",
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
    val base = s"Ratio ${fmtRatio(d.ratio.value)}:1   η ${fmt3(d.efficiency)}   N=${s1.set.numPlanets}"
    val mods = d.stage2 match
      case Some(b) => s"   m₁=${fmt3(s1.module.value)} m₂=${fmt3(b.module.value)}"
      case None    => s"   m=${fmt3(s1.module.value)}"
    val shift = d.profileShiftHint match
      case Some(ProfileShift(x1, x2, alpha)) =>
        f"   shift x₁=$x1%.3f x₂=$x2%.3f α'=$alpha%.2f°"
      case None => ""
    val warn = d.validity match
      case Validity.Warning(reason) => s"   ⚠ $reason"
      case Validity.Ok              => ""
    base + mods + shift + warn

  private def fmt3(d: Double): String =
    val rounded = math.round(d * 1000).toDouble / 1000.0
    rounded.toString

  /** Display a ratio with explicit sign. Positive ratios print as-is; negative
    * ratios (output rotates opposite to input — common with novel couplings)
    * print with a leading "−" + arrow glyph so the user can't mistake a sign
    * flip for a magnitude. */
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
          th(titleAttr := "Even-spacing check: whether all N planets fit symmetrically.", "spacing"),
          th(titleAttr := "Stage 1 sun gear tooth count.", "S₁"),
          th(titleAttr := "Stage 1 planet gear tooth count.", "P₁"),
          th(titleAttr := "Stage 1 ring gear tooth count.", "R₁"),
          th(titleAttr := "Stage 1 module (mm per tooth).", "m₁"),
          th(titleAttr := "Stage 2 sun gear tooth count (only meaningful when stage 2 has its own sun).", "S₂"),
          th(titleAttr := "Stage 2 planet gear tooth count (coupled to the stage-1 planet shaft).", "P₂"),
          th(titleAttr := "Stage 2 ring gear tooth count (the output ring).", "R₂"),
          th(titleAttr := "Stage 2 module (mm per tooth).", "m₂")
        )),
        tbody(children <-- designs.combineWith(displayCount).map { (ds, n) =>
          ds.take(n).map { d =>
            tr(
              cls := "row",
              cls("sel") <-- selected.signal.map(_.contains(d)),
              onClick --> { _ => selected.set(Some(d)) },
              td(cls := "col-warn",
                d.validity match
                  case Validity.Warning(reason) =>
                    span(cls := "warn-glyph", titleAttr := reason, "⚠")
                  case Validity.Ok => span()
              ),
              td(cls := "num", fmtRatio(d.ratio.value)),
              td(cls := "num", fmt3(d.efficiency)),
              td(d.stage1.set.numPlanets.toString),
              td(if d.spacingOk then "ok" else "—"),
              td(if d.topology.s1Present then d.stage1.set.sun.toInt.toString else "—"),
              td(d.stage1.set.planet.toInt.toString),
              td(d.stage1.set.ring.toInt.toString),
              td(cls := "num", fmt3(d.stage1.module.value)),
              td(d.stage2.fold("-") { s =>
                if !d.topology.s2Present then "—" else s.set.sun.toInt.toString
              }),
              td(d.stage2.fold("-")(_.set.planet.toInt.toString)),
              td(d.stage2.fold("-")(_.set.ring.toInt.toString)),
              td(cls := "num", d.stage2.fold("-")(s => fmt3(s.module.value)))
            )
          }
        })
      )
    )

  // ── Inputs ─────────────────────────────────────────────────────────────────

  // ── Topology role-picker UI (D4) ───────────────────────────────────────────
  //
  // Three native <select>s — Input / Output / Fixed — over the bodies present
  // in the current topology, plus a Stages segmented control and an S2-presence
  // toggle. Choosing a body in any picker swaps roles with whoever previously
  // held that role, preserving the "exactly one Input, one Output, one Fixed"
  // invariant. The remaining bodies are Free.

  private def topologyControls(v: Var[Topology]): HtmlElement =
    val stagesSig    = v.signal.map(_.stages)
    val compoundSig  = v.signal.map(t => !UiLogic.couplingSelectorsDisabled(t))
    val inputBody    = v.signal.map(_.input)
    val outputBody   = v.signal.map(_.output)
    val fixedBody    = v.signal.map(_.fixed)
    val s2PresentSig = v.signal.map(_.s2Present)
    val presetSig    = v.signal.map(topologyPreset)
    // Auto-open the Advanced section whenever the topology is `Custom` (no
    // preset match). The user can also toggle manually.
    val advancedOpen: Var[Boolean] = Var(false)
    presetSig.foreach { p =>
      if p == GearboxPreset.Custom then advancedOpen.set(true)
    }(unsafeWindowOwner)

    div(cls := "topology",
      // ── Gearbox-type preset picker — the primary topology control ─────────
      label("Gearbox type"),
      select(cls := "preset-select",
        titleAttr := "Pick a named gearbox family. Most users don't need to touch the Advanced section.",
        onChange.mapToValue --> Observer[String] { name =>
          val p = GearboxPreset.valueOf(name)
          // Custom is a sentinel — selecting it doesn't change the topology;
          // the user adjusts things in the Advanced section to drift off-preset.
          if p != GearboxPreset.Custom then
            safeSet(v, presetTopology(p))
        },
        GearboxPreset.values.toSeq.map { p =>
          option(
            value := p.toString,
            selected <-- presetSig.map(_ == p),
            presetLabel(p)
          )
        }
      ),
      // Plain-English description of the chosen preset.
      div(cls := "topology-derived",
        child.text <-- presetSig.map(presetDescription)
      ),
      // ── Advanced section — couplings, roles, S2 toggle ────────────────────
      div(cls := "advanced-section",
        div(cls := "advanced-summary",
          onClick.preventDefault --> { _ => advancedOpen.update(!_) },
          span(cls := "advanced-arrow",
            child.text <-- advancedOpen.signal.map(if _ then "▾" else "▸")),
          " Advanced topology",
          span(cls := "advanced-customized",
            child.text <-- presetSig.map(p =>
              if p == GearboxPreset.Custom then "  (customized)" else ""
            )
          )
        ),
        div(cls := "advanced-content",
          cls("hidden") <-- advancedOpen.signal.map(!_),
          label("Stages"),
          div(cls := "topology-segment",
            button(
              cls("active") <-- stagesSig.map(_ == 1),
              titleAttr := "One planetary stage: sun + planets + ring + carrier.",
              "Single stage",
              onClick.preventDefault --> { _ => safeSet(v, UiLogic.setStageCount(v.now(), 1)) }
            ),
            button(
              cls("active") <-- stagesSig.map(_ == 2),
              titleAttr := "Two planetary stages sharing a carrier; compound reduction.",
              "Compound (2 stages)",
              onClick.preventDefault --> { _ => safeSet(v, UiLogic.setStageCount(v.now(), 2)) }
            )
          ),
          label(cls := "stage-coupling-row",
            cls("disabled") <-- compoundSig.map(!_),
            "Coupling 1"
          ),
          select(cls := "role-select",
            disabled <-- compoundSig.map(!_),
            titleAttr := "First inter-stage weld. The classic compound uses Carrier 1 ↔ Carrier 2.",
            onChange.mapToValue --> Observer[String] { name =>
              safeSet(v, UiLogic.setCoupling(v.now(), CouplingSlot.First, Coupling.valueOf(name)))
            },
            Coupling.values.toSeq.map { c =>
              option(
                value := c.toString,
                selected <-- v.signal.map(_.firstCoupling == c),
                // Disable conditions:
                //  - same weld as the other dropdown (would make couplings identical);
                //  - PlanetStack option, unless the other dropdown is CarrierToCarrier
                //    (a single stacked pinion physically requires the carriers to be
                //    coupled — see Edge-Case Catalogue #19).
                disabled <-- v.signal.map(UiLogic.couplingOptionDisabled(_, CouplingSlot.First, c)),
                Coupling.label(c)
              )
            }
          ),
          label(cls := "stage-coupling-row",
            cls("disabled") <-- compoundSig.map(!_),
            "Coupling 2"
          ),
          select(cls := "role-select",
            disabled <-- compoundSig.map(!_),
            titleAttr := "Second inter-stage weld. The classic compound uses stacked planets (P1 ↔ P2).",
            onChange.mapToValue --> Observer[String] { name =>
              safeSet(v, UiLogic.setCoupling(v.now(), CouplingSlot.Second, Coupling.valueOf(name)))
            },
            Coupling.values.toSeq.map { c =>
              option(
                value := c.toString,
                selected <-- v.signal.map(_.secondCoupling == c),
                disabled <-- v.signal.map(UiLogic.couplingOptionDisabled(_, CouplingSlot.Second, c)),
                Coupling.label(c)
              )
            }
          ),
          // Manufacturing / bearing-topology note for the chosen welds.
          div(cls := "coupling-note",
            child.text <-- v.signal.map { t =>
              if t.stages != 2 then ""
              else
                val n1 = Coupling.manufacturingNote(t.firstCoupling)
                val n2 = Coupling.manufacturingNote(t.secondCoupling)
                Seq(n1, n2).filter(_.nonEmpty).mkString("  ·  ")
            }
          ),
          label("Input"),
          rolePicker(v, inputBody, BodyRole.Input,
            "Body driven externally (e.g. by a motor)."),
          label("Output"),
          rolePicker(v, outputBody, BodyRole.Output,
            "Body whose speed defines the gearbox ratio."),
          label("Fixed"),
          rolePicker(v, fixedBody, BodyRole.Fixed,
            "Body grounded to the housing (ω = 0). Required to remove the remaining DOF."),
          label(cls := "topology-toggle",
            cls("disabled") <-- compoundSig.map(!_),
            input(typ := "checkbox",
              disabled <-- compoundSig.map(!_),
              controlled(
                checked <-- s2PresentSig,
                onInput.mapToChecked --> Observer[Boolean] { b =>
                  safeSet(v, UiLogic.setS2Present(v.now(), b))
                }
              )
            ),
            span(cls := "switch"),
            span(titleAttr := "When off, stage 2 has no sun gear (NSS). " +
                              "Doesn't change the ratio formula, but expands the search space " +
                              "and removes one mesh worth of friction.",
                 "Stage 2 has its own sun gear")
          ),
          // Legacy derived label inside the advanced section — handy for power
          // users who want the literal role-tuple description.
          div(cls := "topology-derived legacy-derived",
            child.text <-- v.signal.map(t => s"→ ${topologyLabel(t)}")
          )
        )
      )
    )

  /** A native <select> bound to `topology.roles.find(_._2 == targetRole)`. */
  private def rolePicker(
      v: Var[Topology],
      currentSig: Signal[Body],
      targetRole: BodyRole,
      tooltip: String
  ): HtmlElement =
    select(cls := "role-select",
      titleAttr := tooltip,
      onChange.mapToValue --> Observer[String] { bodyName =>
        val b = Body.valueOf(bodyName)
        safeSet(v, UiLogic.assignRole(v.now(), b, targetRole))
      },
      // We render an option for every Body, disabling those that are Absent
      // in the current topology so the user sees the full set with NSS-hidden
      // ones greyed out, not surprisingly missing.
      Body.values.toSeq.map { b =>
        option(
          value := b.toString,
          selected <-- currentSig.map(_ == b),
          disabled <-- v.signal.map { t =>
            // Disable bodies that don't exist in this topology (S2/R2 in single,
            // S2 when NSS), but keep the current selection visible.
            val absentInTop = !t.roles.contains(b) || t.roles.get(b).contains(BodyRole.Absent)
            absentInTop
          },
          Body.label(b)
        )
      }
    )

  private def topologyLabel(t: Topology): String =
    val legacy = t.legacyMode match
      case Some(Mode.Standard)    => "Standard"
      case Some(Mode.CDCP)        => "CDCP"
      case Some(Mode.NSS)         => "NSS"
      case Some(Mode.SingleStage) => "Single"
      case None                   => "custom"
    val desc =
      if t.stages == 1 then s"single-stage, ${Body.label(t.input).toLowerCase} in, " +
                            s"${Body.label(t.output).toLowerCase} out, ${Body.label(t.fixed).toLowerCase} fixed"
      else
        val s2bit = if t.s2Present then "" else " (NSS)"
        s"compound$s2bit: ${Body.label(t.input).toLowerCase} in, " +
        s"${Body.label(t.output).toLowerCase} out, ${Body.label(t.fixed).toLowerCase} fixed"
    s"$desc ($legacy)"

  private def sortPill(v: Var[SortBy], target: SortBy, labelText: String, tooltip: String): HtmlElement =
    button(
      cls := "sort-pill",
      cls("active") <-- v.signal.map(_ == target),
      titleAttr := tooltip,
      labelText,
      onClick.preventDefault --> { _ => v.set(target) }
    )

  /** Min/max range row: label, dual-knob slider (clipped to slider bounds for
    * display only), and two number inputs that accept ANY integer (no
    * clamping). The slider's `min`/`max` attributes limit the drag range, but
    * the user can always type outside that range in the number boxes. If
    * `lo > hi` at search time, `runSearch` silently swaps them before
    * constructing `TeethBounds`.
    */
  private def rangeRow(
      name: String, lo: Var[Int], hi: Var[Int], bounds: (Int, Int),
      tooltip: String = ""
  ): HtmlElement =
    val (absMin, absMax) = bounds

    inline def clipToSlider(v: Int): Int = math.max(absMin, math.min(v, absMax))

    val lbl =
      if tooltip.nonEmpty then label(titleAttr := tooltip, name)
      else                     label(name)
    div(cls := "range-row",
      lbl,
      div(cls := "range-slider",
        div(cls := "range-track"),
        div(cls := "range-track-fill",
          styleAttr <-- lo.signal.combineWith(hi.signal).map { (l, h) =>
            // Clamp display values to slider bounds so the fill bar never
            // overshoots the track when the user types a value out of range.
            val span     = (absMax - absMin).toDouble.max(1.0)
            val loClip   = math.max(absMin, math.min(math.min(l, h), absMax))
            val hiClip   = math.max(absMin, math.min(math.max(l, h), absMax))
            val loFrac   = (loClip - absMin) / span
            val widthFrac = (hiClip - loClip) / span
            // Use calc() that accounts for the thumb width (14px) so the fill
            // bar follows the thumb centres, not the parent edges.
            f"left: calc((100%% - 14px) * $loFrac%.4f + 7px); width: calc((100%% - 14px) * $widthFrac%.4f);"
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

  private def labeled(
      name: String, tooltip: String = "", placeholderText: Option[String] = None
  )(el: HtmlElement): HtmlElement =
    val _ = placeholderText // accepted for caller-side documentation; the el carries its own
    val lbl =
      if tooltip.nonEmpty then label(titleAttr := tooltip, name)
      else                     label(name)
    div(lbl, el)

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
