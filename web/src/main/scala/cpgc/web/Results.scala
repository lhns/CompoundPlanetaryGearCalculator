package cpgc.web

import com.raquo.laminar.api.L.*
import cpgc.domain.{CompoundDesign, Validity}
import cpgc.`export`.{Csv, Json, OpenScad}
import cpgc.search.Scorer.SortBy
import cpgc.web.UiKit.*
import org.scalajs.dom
import java.nio.charset.StandardCharsets
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.{Failure, Success}

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
    val pageSize = Var[Option[Int]](Some(10))
    val pageVar  = Var(0)
    val pageNow  = () => Paging.clamp(pageVar.now(), resultsVar.now().size, pageSize.now())
    // A new search or a re-rank replaces the results: back to page 1.
    val resetPage = resultsVar.signal --> Observer[Vector[CompoundDesign]](_ => pageVar.set(0))
    val clampedPage: Signal[Int] = pageVar.signal.combineWith(resultsVar.signal, pageSize.signal)
      .map((p, ds, s) => Paging.clamp(p, ds.size, s))
    div(cls := "panel results-panel",
      resetPage,
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
          scadMenu(selectedVar)
        )
      ),
      resultsTable(resultsVar.signal, selectedVar, pageSize.signal, clampedPage),
      div(cls := "show-more-row",
        span(cls := "panel-sub", roleAttr := "status",
          child.text <-- resultsVar.signal
            .combineWith(clampedPage, pageSize.signal, statusVar.signal, searchState.signal, searchProgress.signal)
            .map { (ds, page, size, status, state, progress) =>
              state match
                case SearchState.Searching => s"Searching… $progress found"
                case SearchState.Invalid   => "Fix the input error to search."
                case _ =>
                  val countText = Paging.label(page, ds.size, size)
                  if status.isEmpty then countText else s"$countText  ·  $status"
            }
        ),
        div(cls := "show-more-right",
          pager(resultsVar.signal, clampedPage, pageSize.signal, pageVar, pageNow),
          label(cls := "rows-label", forId := "rows-select", "Rows"),
          select(cls := "rows-select", idAttr := "rows-select",
            onChange.mapToValue --> Observer[String] { v =>
              val newSize = if v == "all" then None else v.toIntOption.map(_.max(1))
              if v == "all" || newSize.isDefined then
                pageVar.set(Paging.pageForSizeChange(pageNow(), resultsVar.now().size, pageSize.now(), newSize))
                pageSize.set(newSize)
            },
            RowChoices.map { n =>
              option(value := n.toString,
                     selected <-- pageSize.signal.map(_.contains(n)),
                     n.toString)
            },
            option(value := "all", selected <-- pageSize.signal.map(_.isEmpty), "All")
          )
        )
      )
    )

  private val RowChoices = List(10, 20, 50, 100, 200)

  /** First / previous / next / last buttons; hidden when there is a single page. */
  private def pager(
      results: Signal[Vector[CompoundDesign]], page: Signal[Int], size: Signal[Option[Int]],
      pageVar: Var[Int], pageNow: () => Int
  ): HtmlElement =
    val pages: Signal[Int] = results.combineWith(size).map((ds, s) => Paging.pageCount(ds.size, s))
    def nav(glyph: String, name: String, isOff: Signal[Boolean], target: () => Int): HtmlElement =
      button(cls := "page-btn", typ := "button", ariaLabel := name, titleAttr := name, glyph,
        disabled <-- isOff,
        onClick.preventDefault --> { _ => pageVar.set(target()) })
    val atStart = page.map(_ <= 0)
    val atEnd   = page.combineWith(pages).map((p, n) => p >= n - 1)
    div(cls := "pager", roleAttr := "group", ariaLabel := "Results pages",
      hidden <-- pages.map(_ <= 1),
      nav("«", "First page", atStart, () => 0),
      nav("‹", "Previous page", atStart, () => pageNow() - 1),
      span(cls := "page-indicator", aria.live := "polite",
        child.text <-- page.combineWith(pages).map((p, n) => s"Page ${p + 1} of $n")),
      nav("›", "Next page", atEnd, () => pageNow() + 1),
      nav("»", "Last page", atEnd, () => Int.MaxValue)
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

  // ── SCAD menu ──────────────────────────────────────────────────────────────

  private val FetchTimeoutMs = 10000
  private var libraryCache: Option[String] = None

  /** The pinned gears.scad text. Cached after the first success; fails on network errors,
    * non-200 answers, a timeout, or a body that is not the library. */
  private def fetchLibrary(): Future[String] =
    libraryCache match
      case Some(lib) => Future.successful(lib)
      case None =>
        val ctrl = new dom.AbortController()
        val timer = js.timers.setTimeout(FetchTimeoutMs)(ctrl.abort())
        val init = new dom.RequestInit {}
        init.signal = ctrl.signal
        val result = dom.fetch(OpenScad.LibraryUrl, init).toFuture.flatMap { r =>
          if !r.ok then Future.failed(new RuntimeException(s"HTTP ${r.status}"))
          else r.text().toFuture
        }.flatMap { text =>
          if text.contains("module spur_gear") then { libraryCache = Some(text); Future.successful(text) }
          else Future.failed(new RuntimeException("unexpected content"))
        }
        result.onComplete(_ => js.timers.clearTimeout(timer))
        result

  private def scadName(d: CompoundDesign, ext: String): String =
    s"cpgc-design-${math.abs(d.ratio.value).round}.$ext"

  private def utf8(s: String): Array[Byte] = s.getBytes(StandardCharsets.UTF_8)

  private def scadMenu(selected: Var[Option[CompoundDesign]]): HtmlElement =
    val open = Var(false)
    val busy = Var(false)
    val menuId = freshId("scad-menu")

    /** Fetch the library, then save what `build` makes of it; on failure save the plain script and say so. */
    def withLibrary(d: CompoundDesign)(build: String => Unit): Unit =
      busy.set(true)
      fetchLibrary().onComplete { res =>
        busy.set(false)
        res match
          case Success(lib) => build(lib)
          case Failure(_) =>
            plain(d)
            UiKit.notify("Couldn't fetch gears.scad, saved the .scad alone. " +
              "Get gears.scad from github.com/chrisspen/gears and put it next to the file.", ToastLevel.Warn)
      }
    def plain(d: CompoundDesign): Unit =
      download(scadName(d, "scad"), "application/x-openscad", OpenScad.emit(d))

    def item(text: String, act: CompoundDesign => Unit): HtmlElement =
      button(cls := "menu-item", typ := "button", roleAttr := "menuitem", text,
        onClick.preventDefault --> { _ =>
          open.set(false)
          selected.now().foreach(act)
        })

    var wrap: Option[dom.html.Element] = None
    var trigger: Option[dom.html.Element] = None
    var menuEl: Option[dom.html.Element] = None
    /** Keep the popup inside the viewport (it is right-aligned to the button, which may sit near the left edge), then focus the first item. */
    def placeAndFocus(): Unit =
      js.timers.setTimeout(0) {
        menuEl.foreach { m =>
          m.style.transform = ""
          val r      = m.getBoundingClientRect()
          val vw     = dom.document.documentElement.clientWidth.toDouble
          val margin = 8.0
          val shift  = if r.left < margin then margin - r.left else if r.right > vw - margin then (vw - margin) - r.right else 0.0
          if shift != 0.0 then m.style.transform = s"translateX(${shift}px)"
          Option(m.querySelector(".menu-item")).foreach(_.asInstanceOf[dom.html.Element].focus())
        }
      }

    div(cls := "menu-wrap",
      onMountCallback(c => wrap = Some(c.thisNode.ref)),
      button(
        cls := "export-btn", typ := "button",
        ariaHasPopup := "menu",
        ariaExpanded <-- open.signal.map(_.toString),
        aria.controls := menuId,
        child.text <-- busy.signal.map(b => if b then "SCAD…" else "SCAD ▾"),
        disabled <-- selected.signal.map(_.isEmpty).combineWith(busy.signal).map(_ || _),
        onMountCallback(c => trigger = Some(c.thisNode.ref)),
        onClick.preventDefault --> { _ =>
          open.update(!_)
          if open.now() then placeAndFocus()
        }
      ),
      div(cls := "menu", idAttr := menuId, roleAttr := "menu",
        hidden <-- open.signal.map(!_),
        onMountCallback(c => menuEl = Some(c.thisNode.ref)),
        item("Single file (gears.scad included)", d =>
          withLibrary(d)(lib => download(scadName(d, "scad"), "application/x-openscad", OpenScad.bundle(OpenScad.emit(d), lib)))),
        item("Zip: design + gears.scad", d =>
          withLibrary(d) { lib =>
            val zip = Zip.store(Seq(scadName(d, "scad") -> utf8(OpenScad.emit(d)), "gears.scad" -> utf8(lib)))
            download(scadName(d, "zip"), "application/zip", toUint8Array(zip))
          }),
        item(".scad only (bring your own gears.scad)", plain),
        a(cls := "menu-note", href := OpenScad.LibraryHome, target := "_blank", rel := "noopener",
          OpenScad.LibraryAttribution)
      ),
      onKeyDown.filter(e => e.key == "Escape" && open.now()) --> { e =>
        open.set(false)
        trigger.foreach(_.focus())
      },
      // outside click closes the menu
      documentEvents(_.onClick).filter(e => open.now() && wrap.exists(w => !w.contains(e.target.asInstanceOf[dom.Node])))
        --> { _ => open.set(false) }
    )

  private def toUint8Array(bytes: Array[Byte]): Uint8Array =
    val u = new Uint8Array(bytes.length)
    var i = 0
    while i < bytes.length do { u(i) = (bytes(i) & 0xFF).toShort; i += 1 }
    u

  /** Save `content` as a file. The anchor is attached to the document for the
    * click (Firefox), and the object URL is revoked after the download started. */
  private def download(filename: String, mime: String, content: dom.BlobPart): Unit =
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
      size: Signal[Option[Int]],
      page: Signal[Int]
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
        tbody(children <-- designs.combineWith(size, page).map { (ds, s, p) =>
          val (from, until) = Paging.range(p, ds.size, s)
          ds.slice(from, until).map { d =>
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
