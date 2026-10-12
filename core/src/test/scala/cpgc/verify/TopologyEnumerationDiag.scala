package cpgc.verify

import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.domain.{GearSet, Teeth}
import cpgc.search.RoleSolver
import munit.FunSuite

import java.nio.file.{Files, Paths}

/** ⚠ DIAGNOSTIC / RESEARCH — the printed + written document is the deliverable.
  *
  * Enumerates the entire topology design space (every constructible
  * combination of stages × role tuple × coupling pair), classifies each
  * combination, and groups the survivors into kinematic equivalence classes.
  * Output is written to `docs/topology-enumeration.md` for the next UI plan
  * to lean on.
  *
  * The plan that introduced this is `i-want-you-to-quirky-fog.md` (rev:
  * "Enumerate the full topology design space"). Methodology and caveats are
  * in the generated document.
  */
class TopologyEnumerationDiag extends FunSuite:

  // ── Sample gear pairs ──────────────────────────────────────────────────────
  // Three distinct, even-spaced samples with matching planet counts (so the
  // V3 N1==N2 guard for PlanetStack doesn't reject them). Equivalence classes
  // are detected by ratios agreeing on **all three** samples — two-sample
  // agreement could still be coincidence; three significantly reduces it.
  private val s1A = GearSet(Teeth(24), Teeth(12), 3) // R = 48, (24+48)%3 = 0
  private val s2A = GearSet(Teeth(20), Teeth(16), 3) // R = 52, (20+52)%3 = 0
  private val s1B = GearSet(Teeth(30), Teeth(15), 5) // R = 60, (30+60)%5 = 0
  private val s2B = GearSet(Teeth(18), Teeth(12), 5) // R = 42, (18+42)%5 = 0
  private val s1C = GearSet(Teeth(28), Teeth(14), 4) // R = 56, (28+56)%4 = 0
  private val s2C = GearSet(Teeth(16), Teeth(20), 4) // R = 56, (16+56)%4 = 0

  // ── Classification ─────────────────────────────────────────────────────────
  private sealed trait Classification
  private case object PhysImpossible extends Classification
  private case object Singular extends Classification          // RoleSolver returned None
  private case object Trivial extends Classification           // ratio ≈ 1
  private case object Decoupled extends Classification         // perturbation probe
  private case class Distinct(rA: Double, rB: Double, rC: Double) extends Classification

  private case class Row(
      key: TopologyKey,
      classification: Classification
  )

  private case class TopologyKey(
      stages: Int,
      s2Present: Boolean,
      input: Body, output: Body, fixed: Body,
      firstCoupling: Coupling, secondCoupling: Coupling
  ):
    def rolesStr: String = s"${Body.label(input)} in, ${Body.label(output)} out, ${Body.label(fixed)} fixed"
    def couplingsStr: String =
      if stages == 1 then "—"
      else s"${Coupling.label(firstCoupling)}  +  ${Coupling.label(secondCoupling)}"

  // ── Preset signatures (for cross-referencing) ──────────────────────────────
  // Defined inline so this diag doesn't depend on `web/`.
  private val presetSignatures: Vector[(String, TopologyKey)] = Vector(
    "Single-stage planetary" -> TopologyKey(1, true,  Body.S1, Body.C1, Body.R1,
                                            Coupling.CarrierToCarrier, Coupling.PlanetStack),
    "Sun-driven compound (Standard)" -> TopologyKey(2, true, Body.S1, Body.R2, Body.R1,
                                                    Coupling.CarrierToCarrier, Coupling.PlanetStack),
    "Carrier-driven compound (CDCP)" -> TopologyKey(2, true, Body.C1, Body.R2, Body.R1,
                                                    Coupling.CarrierToCarrier, Coupling.PlanetStack),
    "No-second-sun compound (NSS)" -> TopologyKey(2, false, Body.S1, Body.R2, Body.R1,
                                                  Coupling.CarrierToCarrier, Coupling.PlanetStack),
    "Ring-to-sun cascade" -> TopologyKey(2, true, Body.S1, Body.R2, Body.R1,
                                          Coupling.CarrierToCarrier, Coupling.RingToSun),
    "Ravigneaux" -> TopologyKey(2, true, Body.S1, Body.C2, Body.R1,
                                Coupling.CarrierToSun, Coupling.RingToRing),
    "Simpson" -> TopologyKey(2, true, Body.S1, Body.C2, Body.R1,
                              Coupling.CarrierToRing, Coupling.SunToSun)
  )

  /** Match a TopologyKey against the preset list — order-insensitive on the
    * coupling pair. */
  private def presetName(k: TopologyKey): Option[String] =
    presetSignatures.collectFirst {
      case (n, p) if k.stages == p.stages
                  && k.s2Present == p.s2Present
                  && k.input == p.input && k.output == p.output && k.fixed == p.fixed
                  && Set(k.firstCoupling, k.secondCoupling) == Set(p.firstCoupling, p.secondCoupling)
        => n
    }

  // ── Body sets per stage configuration ──────────────────────────────────────
  private def bodiesFor(stages: Int, s2Present: Boolean): Vector[Body] = stages match
    case 1 => Vector(Body.S1, Body.C1, Body.R1)
    case 2 => if s2Present then Vector(Body.S1, Body.C1, Body.R1, Body.S2, Body.C2, Body.R2)
              else                Vector(Body.S1, Body.C1, Body.R1, Body.R2)

  /** Build a Topology from (stages, s2Present, role tuple, couplings) or return
    * None if the constructor would reject it. */
  private def buildTopology(
      stages: Int, s2Present: Boolean,
      input: Body, output: Body, fixed: Body,
      firstCoupling: Coupling, secondCoupling: Coupling
  ): Option[Topology] =
    val present = bodiesFor(stages, s2Present)
    val pinned = Map(input -> BodyRole.Input, output -> BodyRole.Output, fixed -> BodyRole.Fixed)
    val roles = present.map(b => b -> pinned.getOrElse(b, BodyRole.Free)).toMap
    val withAbsent =
      if stages == 2 && !s2Present then roles.updated(Body.S2, BodyRole.Absent) else roles
    try Some(Topology(stages, withAbsent, firstCoupling, secondCoupling))
    catch case _: IllegalArgumentException => None

  /** Perturb a gear set's planet count by +1 (changes ring via r=s+2p too).
    * Used by the decoupled-stage probe. */
  private def perturb(g: GearSet): GearSet =
    GearSet(g.sun, Teeth(g.planet.toInt + 1), g.numPlanets)

  // ── Intuitive description of a topology ──────────────────────────────────
  // Produces the user's "moving parts as welds, fixed parts as fixed" view:
  //   * compute equivalence classes of bodies under the weld graph
  //   * any class containing the Fixed body → all members are "grounded"
  //   * any class containing the Input body → all members are "driven"
  //   * any class containing the Output body → all members are "at output speed"
  //   * remaining classes with ≥ 2 members → free-moving welded groups
  //   * PlanetStack stays a separate annotation (it's a planet-pinion identity,
  //     not a body-pair weld)

  /** Bodies welded together via the topology's couplings. Each weld is a
    * `(a, b)` body pair from `Coupling.bodyPair`; `PlanetStack` contributes
    * none (it's a derivative equation on planet velocities, not a body weld). */
  private def weldsOf(k: TopologyKey): List[(Body, Body)] =
    if k.stages == 1 then Nil
    else
      List(k.firstCoupling, k.secondCoupling).flatMap(Coupling.bodyPair)

  /** Bodies present given the topology's stage configuration. */
  private def presentBodies(k: TopologyKey): Set[Body] =
    if k.stages == 1 then Set(Body.S1, Body.C1, Body.R1)
    else if k.s2Present then Set(Body.S1, Body.C1, Body.R1, Body.S2, Body.C2, Body.R2)
    else Set(Body.S1, Body.C1, Body.R1, Body.R2)

  /** Union-find equivalence classes of bodies under the weld graph. */
  private def equivalenceClasses(k: TopologyKey): List[Set[Body]] =
    val bodies = presentBodies(k).toVector
    val uf = scala.collection.mutable.Map.empty[Body, Body]
    def find(b: Body): Body = uf.get(b) match
      case None    => b
      case Some(p) =>
        val r = find(p); if r != p then uf(b) = r
        r
    def union(a: Body, b: Body): Unit =
      val ra = find(a); val rb = find(b)
      if ra != rb then uf(ra) = rb
    for (a, b) <- weldsOf(k) if bodies.contains(a) && bodies.contains(b) do union(a, b)
    bodies.groupBy(find).values.map(_.toSet).toList

  private def bodyShort(b: Body): String = b match
    case Body.S1 => "S1"; case Body.C1 => "C1"; case Body.R1 => "R1"
    case Body.S2 => "S2"; case Body.C2 => "C2"; case Body.R2 => "R2"

  /** Classify a single (k, topology) — see classification ADT above. */
  private def classify(k: TopologyKey, t: Topology): Classification =
    // PlanetStack physical-impossibility check (kinematic chimera).
    val hasPS = k.firstCoupling == Coupling.PlanetStack || k.secondCoupling == Coupling.PlanetStack
    val hasCC = k.firstCoupling == Coupling.CarrierToCarrier || k.secondCoupling == Coupling.CarrierToCarrier
    if hasPS && !hasCC && k.stages == 2 then return PhysImpossible

    val s2OptA = if k.stages == 1 then None else Some(s2A)
    val s2OptB = if k.stages == 1 then None else Some(s2B)
    val s2OptC = if k.stages == 1 then None else Some(s2C)
    val rA = RoleSolver.ratio(t, s1A, s2OptA).map(_.value)
    val rB = RoleSolver.ratio(t, s1B, s2OptB).map(_.value)
    val rC = RoleSolver.ratio(t, s1C, s2OptC).map(_.value)
    (rA, rB, rC) match
      case (Some(a), Some(b), Some(c)) =>
        if math.abs(a - 1.0) < 1e-9 && math.abs(b - 1.0) < 1e-9 && math.abs(c - 1.0) < 1e-9 then Trivial
        else
          // Decoupled probe on sample A (cheap; if decoupled on A it's
          // structurally decoupled regardless of sample).
          val s2Decoupled = k.stages == 2 &&
            RoleSolver.ratio(t, s1A, s2OptA.map(perturb)).map(_.value)
              .exists(rp => math.abs(rp - a) < 1e-9)
          val s1Decoupled = k.stages == 2 &&
            RoleSolver.ratio(t, perturb(s1A), s2OptA).map(_.value)
              .exists(rp => math.abs(rp - a) < 1e-9)
          if s2Decoupled || s1Decoupled then Decoupled
          else Distinct(a, b, c)
      case _ => Singular

  // ── Enumeration ────────────────────────────────────────────────────────────
  private def enumerateAll(): Vector[Row] =
    // All ordered (a, b) with a < b lexicographically — i.e. distinct unordered
    // coupling pairs in a canonical order. The `Topology` invariant only
    // requires they differ, so we won't bother with both orderings (they yield
    // the same kinematics — coupling rows are symmetric).
    val couplings = Coupling.values.toVector
    val allPairs =
      for
        i <- couplings.indices.toVector
        j <- (i + 1) until couplings.length
      yield (couplings(i), couplings(j))

    val rows = scala.collection.mutable.ArrayBuffer.empty[Row]

    // Single-stage: no couplings, but use canonical placeholders for the key.
    val ss = bodiesFor(1, true)
    for in <- ss; out <- ss if out != in; fx <- ss if fx != in && fx != out do
      val k = TopologyKey(1, true, in, out, fx, Coupling.CarrierToCarrier, Coupling.PlanetStack)
      buildTopology(1, true, in, out, fx, Coupling.CarrierToCarrier, Coupling.PlanetStack).foreach { t =>
        rows += Row(k, classify(k, t))
      }

    // NSS (compound, S2 absent): couplings don't influence the NSS solver
    // path (it emits its own combined row); enumerate role tuples only,
    // with the canonical {CC, PS} pair.
    val nss = bodiesFor(2, false)
    for in <- nss; out <- nss if out != in; fx <- nss if fx != in && fx != out do
      val k = TopologyKey(2, false, in, out, fx, Coupling.CarrierToCarrier, Coupling.PlanetStack)
      buildTopology(2, false, in, out, fx, Coupling.CarrierToCarrier, Coupling.PlanetStack).foreach { t =>
        rows += Row(k, classify(k, t))
      }

    // Full compound: 120 role tuples × 45 distinct coupling pairs = 5 400.
    val fc = bodiesFor(2, true)
    for in <- fc; out <- fc if out != in; fx <- fc if fx != in && fx != out do
      for (c1, c2) <- allPairs do
        val k = TopologyKey(2, true, in, out, fx, c1, c2)
        buildTopology(2, true, in, out, fx, c1, c2).foreach { t =>
          rows += Row(k, classify(k, t))
        }

    rows.toVector

  // ── Structural fingerprint ────────────────────────────────────────────────
  // A "structural model" is identified by *what is welded to what*, not by
  // numerical ratios. Two topologies are the same model if they share:
  //   - stage count (1 vs 2)
  //   - whether S2 is present
  //   - the equivalence class containing the Input (set of "driven" bodies)
  //   - the equivalence class containing the Output (set of "output-speed" bodies)
  //   - the equivalence class containing the Fixed body (set of "grounded" bodies)
  //   - the set of remaining ≥2-member free welds
  //   - whether stacked planets (PlanetStack) are part of the topology
  //
  // Different role tuples that produce the same partition land in the same
  // model. Different coupling pairs that produce the same weld graph also
  // land in the same model.

  private case class StructuralKey(
      stages: Int,
      s2Present: Boolean,
      driven: Set[Body],
      output: Set[Body],
      grounded: Set[Body],
      freeWelds: Set[Set[Body]],
      stackedPlanets: Boolean
  )

  private def structuralKey(k: TopologyKey): StructuralKey =
    val classes = equivalenceClasses(k)
    val driven   = classes.find(_.contains(k.input)).getOrElse(Set(k.input))
    val output   = classes.find(_.contains(k.output)).getOrElse(Set(k.output))
    val grounded = classes.find(_.contains(k.fixed)).getOrElse(Set(k.fixed))
    val pinned   = Set(driven, output, grounded)
    val freeWelds = classes.filter(c => c.size >= 2 && !pinned.contains(c)).toSet
    val hasPS = k.stages == 2 &&
      (k.firstCoupling == Coupling.PlanetStack || k.secondCoupling == Coupling.PlanetStack)
    StructuralKey(k.stages, k.s2Present, driven, output, grounded, freeWelds, hasPS)

  // Stage-1 preferred for input, stage-2 for output. Within a stage S < C < R.
  private inline def inputOrder(b: Body): Int = b match
    case Body.S1 => 0; case Body.C1 => 1; case Body.R1 => 2
    case Body.S2 => 3; case Body.C2 => 4; case Body.R2 => 5
  private inline def outputOrder(b: Body): Int = b match
    case Body.S2 => 0; case Body.C2 => 1; case Body.R2 => 2
    case Body.S1 => 3; case Body.C1 => 4; case Body.R1 => 5

  /** Structured, canonical decomposition of a `StructuralKey` for display
    * and CSV emission. */
  private case class Canonical(
      input: Body,
      output: Body,
      fixed: Vector[Body],         // sorted by bodyShort
      welds: Vector[Vector[Body]]  // each sorted; outer sorted by joined string
  )

  private def canonicalize(sk: StructuralKey): Canonical =
    val inputPick  = sk.driven.toVector.minBy(inputOrder)
    val outputPick = sk.output.toVector.minBy(outputOrder)
    val fixedSorted = sk.grounded.toVector.sortBy(bodyShort)
    val weldsBuf = scala.collection.mutable.ArrayBuffer.empty[Set[Body]]
    if sk.driven.size > 1 then weldsBuf += sk.driven
    if sk.output.size > 1 && sk.output != sk.driven then weldsBuf += sk.output
    weldsBuf ++= sk.freeWelds.toVector
    val weldsSorted = weldsBuf.toVector
      .map(_.toVector.sortBy(bodyShort))
      .sortBy(_.map(bodyShort).mkString("+"))
    Canonical(inputPick, outputPick, fixedSorted, weldsSorted)

  private def describeStructure(sk: StructuralKey): String =
    val c = canonicalize(sk)
    inline def join(bs: Vector[Body]): String = bs.map(bodyShort).mkString("+")
    val parts = scala.collection.mutable.ArrayBuffer.empty[String]
    parts += s"input: ${bodyShort(c.input)}"
    parts += s"output: ${bodyShort(c.output)}"
    parts += s"fixed: ${join(c.fixed)}"
    for w <- c.welds do parts += s"weld: ${join(w)}"
    if sk.stackedPlanets then parts += "stacked planets"
    parts.mkString("; ")

  // ── Document emitter ───────────────────────────────────────────────────────
  private def buildDoc(rows: Vector[Row]): String =
    val total = rows.size
    val physImpossible = rows.filter(_.classification == PhysImpossible)
    val viable = rows.filter(r =>
      r.classification != PhysImpossible)

    // Group every viable row by its structural fingerprint — *no ratios*.
    // Each group is a distinct structural model: a unique answer to
    // "what is driving what, and what is fixed".
    val grouped: Map[StructuralKey, Vector[Row]] =
      viable.groupBy(r => structuralKey(r.key))

    // Per-model classification breakdown — does the model produce a valid
    // gearbox, or do its kinematics collapse?
    final case class ModelSummary(
        key: StructuralKey,
        members: Vector[Row]
    ):
      def works: Boolean = members.exists(_.classification.isInstanceOf[Distinct])
      def allDegenerate: Boolean = !works
      def classBreakdown: String =
        val counts = members.groupBy(_.classification).view.mapValues(_.size).toMap
        val parts = scala.collection.mutable.ArrayBuffer.empty[String]
        counts.get(Singular).foreach(n => parts += s"$n singular")
        counts.get(Trivial).foreach(n => parts += s"$n trivial")
        counts.get(Decoupled).foreach(n => parts += s"$n decoupled")
        val distinctCount = members.count(_.classification.isInstanceOf[Distinct])
        if distinctCount > 0 then parts.insert(0, s"$distinctCount works")
        parts.mkString(", ")

    val models = grouped.toVector.map { (sk, rs) => ModelSummary(sk, rs) }

    // Sort: working models first (by member count desc), then degenerate.
    val working    = models.filter(_.works).sortBy(m => -m.members.size)
    val degenerate = models.filter(_.allDegenerate).sortBy(m => -m.members.size)

    val sb = StringBuilder()
    sb ++= "# Topology design-space enumeration — structural view\n\n"
    sb ++= "Auto-generated by `cpgc.verify.TopologyEnumerationDiag`. Re-run with\n"
    sb ++= "`sbt \"coreJVM/testOnly cpgc.verify.TopologyEnumerationDiag\"` to regenerate.\n\n"
    sb ++= "**This document lists *structural models* — what is driving what, what is fixed, what is welded together.** Numerical gear ratios are *not* the grouping key here; the question being answered is \"what are the distinct mechanical arrangements the 6-body + 10-coupling model can express?\"\n\n"

    sb ++= "## Methodology\n\n"
    sb ++= "- Body set: `{S1, C1, R1, S2, C2, R2}` for full compounds, `{S1, C1, R1, R2}` for NSS, `{S1, C1, R1}` for single-stage.\n"
    sb ++= "- Role tuple = `(Input, Output, Fixed)`, three distinct bodies. Remaining bodies are Free (or Absent for S2 in NSS).\n"
    sb ++= "- Coupling pair = two distinct entries from the 10-coupling enum (45 unordered pairs).\n"
    sb ++= "- For each constructible topology, compute equivalence classes of bodies under the weld graph (couplings are welds; `PlanetStack` is noted separately because it equates planet pinions, not central shafts).\n"
    sb ++= "- **Structural fingerprint** = (stage count, S2-present, driven class, output class, grounded class, set of free welds, stacked-planets flag). Two topologies belong to the same model when their fingerprints match exactly.\n"
    sb ++= "- A working model has at least one constructible underlying topology that produces a finite, non-trivial, fully-coupled ratio. A degenerate model is one whose every underlying topology collapses to singular, trivial passthrough, or stage-decoupled.\n"
    sb ++= "- `PlanetStack` paired with anything other than `CarrierToCarrier` is excluded as physically impossible (a stacked pinion can't sit at two orbital positions).\n\n"

    sb ++= "## Headline numbers\n\n"
    sb ++= s"- **Total constructible combinations enumerated**: $total\n"
    sb ++= s"- **Excluded as physically impossible** (PlanetStack ⊕ non-CC): ${physImpossible.size}\n"
    sb ++= s"- **Distinct structural models**: ${models.size}\n"
    sb ++= s"  - working (at least one underlying topology yields a valid gearbox): **${working.size}**\n"
    sb ++= s"  - degenerate (every underlying topology singular / trivial / decoupled): **${degenerate.size}**\n\n"

    sb ++= "## Working structural models\n\n"
    sb ++= s"All ${working.size} structural arrangements the model can express that yield a valid, non-trivial gearbox.\n\n"
    sb ++= "Reading the **What it does** column:\n"
    sb ++= "- `input: X` — the single input shaft you connect a motor to. Stage-1 bodies are preferred; if another body is welded to the input it appears under `weld:` below.\n"
    sb ++= "- `output: X` — the single output shaft. Stage-2 bodies are preferred; welded peers appear under `weld:` below.\n"
    sb ++= "- `fixed: X+Y+...` — all bodies welded to the case/ground, listed together.\n"
    sb ++= "- `weld: X+Y` — a group of bodies rigidly tied together (one of the couplings). The input's own welded peers and the output's own welded peers also show up here.\n"
    sb ++= "- `stacked planets` — the two stages share a single stepped pinion (`PlanetStack` is one of the couplings).\n\n"
    sb ++= "Note: configurations that differ only in which member of a welded group was *labeled* the input (e.g. \"S1 is input, S2 welded to S1\" vs. \"S2 is input, S1 welded to S2\") collapse to the same row — they are the same machine.\n\n"
    sb ++= "| # | What it does | Underlying combos | Classification breakdown | Preset(s) |\n"
    sb ++= "|---|---|---:|---|---|\n"
    for (m, idx) <- working.zipWithIndex do
      val desc = describeStructure(m.key)
      val presets = m.members.iterator.flatMap(r => presetName(r.key)).toVector.distinct.mkString(" / ")
      sb ++= s"| ${idx + 1} | $desc | ${m.members.size} | ${m.classBreakdown} | $presets |\n"

    sb ++= "\n## Degenerate structural models\n\n"
    sb ++= s"${degenerate.size} structural arrangements that the model can write down but that always collapse to a degenerate kinematic system. Listed here so we know they exist — they should not be exposed in the UI.\n\n"
    sb ++= "| # | What it does | Underlying combos | Why it's degenerate |\n"
    sb ++= "|---|---|---:|---|\n"
    for (m, idx) <- degenerate.zipWithIndex do
      val desc = describeStructure(m.key)
      sb ++= s"| ${idx + 1} | $desc | ${m.members.size} | ${m.classBreakdown} |\n"

    sb ++= "\n## Preset cross-reference\n\n"
    sb ++= "Which structural model does each named preset land in?\n\n"
    sb ++= "| Preset | Model # | Description | Shares model with |\n"
    sb ++= "|---|---:|---|---|\n"
    for (name, _) <- presetSignatures do
      val found = working.zipWithIndex.collectFirst {
        case (m, i) if m.members.exists(r => presetName(r.key).contains(name)) => (m, i + 1)
      }
      found match
        case Some((m, idx)) =>
          val others = m.members.iterator.flatMap(r => presetName(r.key)).toVector.distinct.filterNot(_ == name)
          val sharesWith = if others.isEmpty then "—" else others.mkString(", ")
          sb ++= s"| **$name** | $idx | ${describeStructure(m.key)} | $sharesWith |\n"
        case None =>
          sb ++= s"| **$name** | — | (no working model matched) | — |\n"

    sb ++= "\n## Physically impossible — listed for completeness\n\n"
    sb ++= "`PlanetStack` paired with any coupling other than `CarrierToCarrier`. A single stacked pinion can't simultaneously sit at two different orbital positions, so the carriers must be coupled. The kinematic model permits these pairs but no machine realizes them. The UI prevents them.\n\n"
    val physGrouped = physImpossible.groupBy(r => (r.key.firstCoupling, r.key.secondCoupling))
    for ((c1, c2), rs) <- physGrouped.toVector.sortBy(_._1.toString) do
      sb ++= s"- ${Coupling.label(c1)}  +  ${Coupling.label(c2)} — ${rs.size} role-tuple combinations\n"

    sb.result()

  // ── CSV emitter ───────────────────────────────────────────────────────────
  // One row per *working* structural model, columns: input, output, fixed,
  // welds, stacked_planets, stages, s2_present, underlying_combos, preset.
  // Sorted by (input, output, fixed, welds) using the canonical body order.
  private def buildCsv(rows: Vector[Row]): String =
    val viable = rows.filter(_.classification != PhysImpossible)
    val grouped = viable.groupBy(r => structuralKey(r.key))
    val working = grouped.toVector
      .filter { case (_, rs) => rs.exists(_.classification.isInstanceOf[Distinct]) }

    // Decorate each model with its canonical decomposition for sorting.
    val decorated = working.map { case (sk, rs) =>
      val c = canonicalize(sk)
      val presets = rs.iterator.flatMap(r => presetName(r.key)).toVector.distinct.mkString(" / ")
      (sk, c, rs, presets)
    }

    // Stable sort key: body-order int for input, then output, then the
    // fixed-list as a tuple of body-orders, then the welds-list likewise.
    inline def bodyAxis(b: Body): Int = inputOrder(b) // canonical body order
    def vecKey(bs: Vector[Body]): Vector[Int] = bs.map(bodyAxis)
    def weldsKey(ws: Vector[Vector[Body]]): Vector[Vector[Int]] = ws.map(vecKey)
    val sorted = decorated.sortBy { case (_, c, _, _) =>
      (bodyAxis(c.input), bodyAxis(c.output), vecKey(c.fixed).mkString(","), weldsKey(c.welds).map(_.mkString(",")).mkString(";"))
    }

    inline def joinBodies(bs: Vector[Body]): String = bs.map(bodyShort).mkString("+")
    inline def joinWelds(ws: Vector[Vector[Body]]): String = ws.map(joinBodies).mkString("; ")

    val sb = StringBuilder()
    sb ++= "input,output,fixed,welds,stacked_planets,stages,s2_present,underlying_combos,preset\n"
    for (sk, c, rs, presets) <- sorted do
      val fields = Vector(
        bodyShort(c.input),
        bodyShort(c.output),
        joinBodies(c.fixed),
        joinWelds(c.welds),
        if sk.stackedPlanets then "yes" else "no",
        sk.stages.toString,
        if sk.s2Present then "yes" else "no",
        rs.size.toString,
        presets
      )
      // CSV-escape: quote fields containing comma, quote, or newline.
      val escaped = fields.map { f =>
        if f.contains(",") || f.contains("\"") || f.contains("\n") then
          "\"" + f.replace("\"", "\"\"") + "\""
        else f
      }
      sb ++= escaped.mkString(",")
      sb ++= "\n"
    sb.result()

  // ── The diagnostic test ────────────────────────────────────────────────────
  test("DIAG: enumerate the full topology design space → docs/topology-enumeration.md") {
    val rows = enumerateAll()
    val doc = buildDoc(rows)
    val csv = buildCsv(rows)
    val mdPath  = Paths.get("docs/topology-enumeration.md")
    val csvPath = Paths.get("docs/topology-enumeration.csv")
    // Ensure the docs/ directory exists (it does in the repo, but be defensive).
    Files.createDirectories(mdPath.getParent)
    Files.writeString(mdPath, doc)
    Files.writeString(csvPath, csv)
    println(s"Wrote ${doc.length} chars to ${mdPath.toAbsolutePath}")
    println(s"Wrote ${csv.length} chars to ${csvPath.toAbsolutePath}")
    assert(doc.length > 1000, "expected a non-trivial document")
    assert(csv.length > 1000, "expected a non-trivial CSV")
    assert(rows.size > 5000, s"expected >5000 enumerated combinations, got ${rows.size}")
  }
