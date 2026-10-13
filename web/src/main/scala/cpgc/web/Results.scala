package cpgc.web

import com.raquo.laminar.api.L.*
import cpgc.domain.{CompoundDesign, Validity}
import cpgc.`export`.{Csv, Json, OpenScad}
import cpgc.search.Scorer.SortBy
import cpgc.web.UiKit.*
import org.scalajs.dom
import scala.scalajs.js

// Results panel: sort pills, export buttons, the results table, its detail line and the row-count select.
object Results:

  def panel(
      resultsVar: Var[Vector[CompoundDesign]],
      selectedVar: Var[Option[CompoundDesign]],
      sortByVar: Var[SortBy],
      hasTargets: Signal[Boolean],
      chooseSort: SortBy => Unit,
      statusVar: Var[String],
      searchState: Var[SearchState],
      searchProgress: Var[Int]
  ): HtmlElement =
    val displayCount = Var(10)
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

  def detailLine(d: CompoundDesign): String =
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
