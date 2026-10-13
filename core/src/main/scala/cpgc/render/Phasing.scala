package cpgc.render

import cpgc.domain.{CompoundDesign, Stage}

/** Tooth phasing of a design: the rotations that make every gear mesh at every
  * planet position (docs/verification/assembly-conditions.md). Shared by the
  * OpenSCAD export and the SVG schematic, so what is drawn is what is exported.
  *
  * Angles are in turns, in a frame where planet k sits at θ_k = k/N, and are
  * the angle of a *tooth centre*: an external gear with `T` teeth at angle `a`
  * has phase `frac(T·(θ − a))` towards direction θ (0 = tooth centred). A
  * renderer with another origin adds the same constant to every angle; the
  * mesh conditions are invariant under that.
  *
  *   sun :  Ts·(θ_k − σ) + Tp·(θ_k + ½ − x_k) ≡ ½
  *   ring:  Tr·(θ_k − ρ) − Tp·(θ_k − x_k)     ≡ ½
  *
  * Planet 0 is pinned at x_0 = 0, which fixes σ and ρ. Each further planet
  * then needs Tp·x_k ≡ rhs_k (mod 1) for every congruence it takes part in. A
  * stacked pinion that is the same part for every planet shares one x_k
  * between both stages and must satisfy both stages' congruences, which is
  * possible exactly when the assembly condition of the design holds. With
  * `phasedPinions` (ADR 0018) each half is solved on its own and the planet's
  * half 2 is twisted by `g_k = x2_k − x1_k` relative to half 1.
  */
object Phasing:

  /** `sun`: sun tooth-centre angle (None for a sun-less stage). `ring`: angle of a ring
    * *material* tooth centre. `planets`: planet body angles x_k (a tooth centre). */
  final case class StagePhasing(sun: Option[Double], ring: Double, planets: Vector[Double])

  /** `meshes` is false when some planet has no body angle satisfying all its
    * congruences (the assembly condition is violated; angles are best-effort).
    * `twists` (turns in [0, 1)) is set for phased stacked designs: half 2 of
    * planet k is rotated by `twists(k)` relative to half 1. */
  final case class Result(
      stage1: StagePhasing, stage2: Option[StagePhasing], meshes: Boolean, twists: Option[Vector[Double]]
  )

  /** Tp·x ≡ rhs(k) (mod 1). */
  private final case class Cong(tp: Int, rhs: Int => Double)

  private final case class Plan(sun: Option[Double], ring: Double, congs: Seq[Cong], n: Int)

  private val Tol = 1e-6

  private def isInteger(v: Double): Boolean =
    val f = v - math.floor(v)
    f < Tol || f > 1.0 - Tol

  /** Fractional part, with values within 1e-9 of 0 or 1 snapped to 0. */
  def fracSnap(v: Double): Double =
    val f = v - math.floor(v)
    if f < 1e-9 || f > 1.0 - 1e-9 then 0.0 else f

  private def plan(s: Stage, sunPresent: Boolean): Plan =
    val (ts, tp, tr, n) = (s.set.sun.toInt, s.set.planet.toInt, s.set.ring.toInt, s.set.numPlanets)
    val sigma = (tp / 2.0 - 0.5) / ts
    val rho   = -0.5 / tr
    val ring  = Cong(tp, k => { val th = k.toDouble / n; 0.5 - tr * (th - rho) + tp * th })
    val sun   = Cong(tp, k => { val th = k.toDouble / n; ts * (th - sigma) + tp * (th + 0.5) - 0.5 })
    Plan(
      sun   = if sunPresent then Some(sigma) else None,
      ring  = rho,
      congs = if sunPresent then Seq(ring, sun) else Seq(ring),
      n     = n
    )

  /** Planet body angles and whether every planet could be phased. */
  private def solvePlanets(congs: Seq[Cong], n: Int): (Vector[Double], Boolean) =
    val first = congs.head
    val xs = (0 until n).map { k =>
      val base = fracSnap(first.rhs(k)) / first.tp
      (0 until first.tp).map(j => base + j.toDouble / first.tp)
        .find(x => congs.forall(c => isInteger(c.tp * x - c.rhs(k))))
        .map(_ -> true).getOrElse(base -> false)
    }
    (xs.map(_._1).toVector, xs.forall(_._2))

  def solve(d: CompoundDesign): Result =
    val t  = d.topology
    val p1 = plan(d.stage1, t.s1Present)
    val p2 = d.stage2.map(plan(_, t.s2Present))
    def phasing(p: Plan, xs: Vector[Double]) = StagePhasing(p.sun, p.ring, xs)
    p2 match
      case Some(q) if t.stacked && !d.phasedPinions =>
        // one identical stepped pinion per planet: a single body angle must satisfy both stages
        val (x, ok) = solvePlanets(p1.congs ++ q.congs, p1.n)
        Result(phasing(p1, x), Some(phasing(q, x)), ok, None)
      case Some(q) =>
        val (x1, o1) = solvePlanets(p1.congs, p1.n)
        val (x2, o2) = solvePlanets(q.congs, q.n)
        val twists =
          if t.stacked then Some(x1.zip(x2).map((a, b) => fracSnap(b - a))) else None
        Result(phasing(p1, x1), Some(phasing(q, x2)), o1 && o2, twists)
      case None =>
        val (x1, o1) = solvePlanets(p1.congs, p1.n)
        Result(phasing(p1, x1), None, o1, None)
