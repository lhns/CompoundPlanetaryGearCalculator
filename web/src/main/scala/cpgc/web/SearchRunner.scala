package cpgc.web

import cpgc.Calculator
import cpgc.constraints.SearchConfig
import cpgc.domain.CompoundDesign
import cpgc.search.Scorer
import cpgc.search.Scorer.SortBy
import com.raquo.laminar.api.L.Var
import cpgc.web.SearchRunner.*
import scala.scalajs.js

// Chunked, time-capped search over the engine; reports through the Vars it is given.
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

/** What the last finished search produced. `designs` is the complete result
  * set when `rerankable` (so a sort change needs no new search). */
final case class FinishedRun(cfg: SearchConfig, designs: Vector[CompoundDesign], rerankable: Boolean)

final class SearchRunner(
    formNow: () => FormValues,
    sortByVar: Var[SortBy],
    resultsVar: Var[Vector[CompoundDesign]],
    selectedVar: Var[Option[CompoundDesign]],
    statusVar: Var[String],
    errorVar: Var[Option[String]],
    searchState: Var[SearchState],
    searchProgress: Var[Int],
    searchProgressFraction: Var[Double],
    writeUrl: (FormValues, SortBy) => Unit
):
  private var lastRun = Option.empty[FinishedRun]
  // Cancellation token: each search bumps the epoch; in-flight chunks of an
  // older search exit on their next iteration.
  private var searchEpoch = 0L

  private def clearResults(): Unit =
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

  private def startSearch(epoch: Long, cfg: SearchConfig): Unit =
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

object SearchRunner:

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
