package cpgc.verify

import java.lang.Math.{addExact, floorMod, multiplyExact}

/** Exact rational number. Used for angles in turns and tooth phases in pitch
  * units, so mesh checks are exact equalities with no tolerance. Arithmetic
  * throws on Long overflow instead of wrapping. */
final case class Rat(n: Long, d: Long):
  def +(o: Rat): Rat =
    val g = Rat.gcd(d, o.d)
    val l = multiplyExact(d / g, o.d)
    Rat.of(addExact(multiplyExact(n, l / d), multiplyExact(o.n, l / o.d)), l)
  def unary_- : Rat      = Rat(-n, d)
  def -(o: Rat): Rat     = this + (-o)
  def *(k: Long): Rat    = Rat.of(multiplyExact(n, k), d)
  def /(k: Long): Rat    = Rat.of(n, multiplyExact(d, k))
  /** `this mod 1`, in [0, 1). */
  def frac: Rat          = Rat(floorMod(n, d), d)

object Rat:
  def gcd(a: Long, b: Long): Long = if b == 0 then math.abs(a) else gcd(b, a % b)
  def of(n: Long, d: Long): Rat =
    require(d != 0, "zero denominator")
    val s = if d < 0 then -1 else 1
    val g = gcd(n, d).max(1)
    Rat(s * n / g, s * d / g)
  def apply(n: Long): Rat = Rat(n, 1)
  val Zero: Rat           = Rat(0, 1)
  val Half: Rat           = Rat(1, 2)

/** Independent brute-force tooth-phase oracle for planet assembly.
  *
  * Model (everything in the carrier frame unless stated; angles in turns):
  *  - a gear with `T` teeth and rotation `rot` has tooth centres at
  *    `rot + j/T`. Its phase seen in direction `dir` from its own centre is
  *    `frac(T·(dir − rot))`: 0 when a tooth is centred on `dir`.
  *  - planet k of a stage with N planets sits at direction `φ + k/N`, where `φ`
  *    is the carrier angle. Its contact with the sun lies on the line of
  *    centres: direction `θ` seen from the sun, `θ + ½` seen from the planet.
  *    Its contact with the ring is direction `θ` from both centres.
  *  - mesh: along the common tangent at the pitch point a tooth of one gear
  *    must sit centred in a gap of the other. For an external pair the CCW
  *    tangents of the two gears are opposite, so tooth positions along the
  *    tangent are `−u_a` and `+u_b` (pitch units) and the condition is
  *    `u_a + u_b ≡ ½`. For an internal pair the tangents agree and the
  *    condition is `u_ring − u_planet ≡ ½`.
  *
  * Nothing here uses `(S + R) % N` or any production spacing code. The search
  * is exhaustive over the discrete solution sets of each mesh (see
  * `stacked`), so a `false` is a proof of non-assemblability for the model,
  * not a sampling miss.
  */
object AssemblyOracle:
  import Rat.Half

  /** Tooth counts of one stage. `sun = None` means the stage has no sun gear
    * (its planets mesh only the ring). */
  final case class Stage(sun: Option[Int], planet: Int, ring: Int):
    def withoutSun: Stage = copy(sun = None)

  object Stage:
    /** Stage with `ring = sun + 2·planet`, the calculator's convention. */
    def std(sun: Int, planet: Int): Stage = Stage(Some(sun), planet, sun + 2 * planet)

  /** How the two halves of a stacked pinion are phased relative to each other. */
  enum Pinion:
    /** Every planet's half 2 is rotated by `g` turns relative to its half 1,
      * the same `g` for all planets (one-piece / 3D-printed stepped planets). */
    case Identical(g: Rat)
    /** Each planet's halves are phased individually (N different pinions). */
    case Individual

  def phase(teeth: Int, rot: Rat, dir: Rat): Rat = ((dir - rot) * teeth).frac
  def externalMesh(ua: Rat, ub: Rat): Boolean   = (ua + ub).frac == Half
  def internalMesh(ur: Rat, up: Rat): Boolean   = (ur - up).frac == Half

  def sunMeshOk(ts: Int, sunRot: Rat, tp: Int, halfRot: Rat, dir: Rat): Boolean =
    externalMesh(phase(ts, sunRot, dir), phase(tp, halfRot, dir + Half))
  def ringMeshOk(tr: Int, ringRot: Rat, tp: Int, halfRot: Rat, dir: Rat): Boolean =
    internalMesh(phase(tr, ringRot, dir), phase(tp, halfRot, dir))

  // Inverses of the mesh conditions: one representative solution, valid modulo
  // one tooth pitch of the gear being solved for.
  private def sunFromPlanet(ts: Int, tp: Int, halfRot: Rat, dir: Rat): Rat =
    val us = (Half - phase(tp, halfRot, dir + Half)).frac
    dir - us / ts
  private def ringFromPlanet(tr: Int, tp: Int, halfRot: Rat, dir: Rat): Rat =
    val ur = (Half + phase(tp, halfRot, dir)).frac
    dir - ur / tr
  /** Every planet rotation (over a full turn) that meshes with the given sun. */
  private def planetsFromSun(ts: Int, sunRot: Rat, tp: Int, dir: Rat): Iterator[Rat] =
    val up = (Half - phase(ts, sunRot, dir)).frac
    Iterator.range(0, tp).map(j => dir + Half - (up + Rat(j)) / tp)
  /** Every planet rotation (over a full turn) that meshes with the given ring. */
  private def planetsFromRing(tr: Int, ringRot: Rat, tp: Int, dir: Rat): Iterator[Rat] =
    val up = (phase(tr, ringRot, dir) - Half).frac
    Iterator.range(0, tp).map(j => dir - (up + Rat(j)) / tp)

  /** Central-gear rotations of one stage. */
  final case class Centrals(sun: Option[Rat], ring: Rat)

  private def halfMeshes(st: Stage, c: Centrals, halfRot: Rat, dir: Rat): Boolean =
    st.sun.zip(c.sun).forall((ts, s) => sunMeshOk(ts, s, st.planet, halfRot, dir)) &&
      ringMeshOk(st.ring, c.ring, st.planet, halfRot, dir)

  private def candidates(st: Stage, c: Centrals, dir: Rat): Iterator[Rat] =
    (st.sun, c.sun) match
      case (Some(ts), Some(s)) => planetsFromSun(ts, s, st.planet, dir)
      case _                   => planetsFromRing(st.ring, c.ring, st.planet, dir)

  /** Can N identical, equally spaced planets be assembled? `stages` are the
    * pinion halves sharing one planet body: one entry for a single stage, two
    * for a stacked (stepped-planet) compound on one carrier.
    *
    * Search (exact and exhaustive):
    *  1. With the carrier held, the gear train still has one degree of freedom
    *     that turns planet 0, and turning it preserves every mesh. So planet 0's
    *     body angle can be fixed to `x0` without losing solutions (`x0` is a
    *     parameter so tests can confirm the verdict does not depend on it).
    *  2. Each central gear meshes planet 0, which fixes its rotation up to one
    *     of its own tooth pitches, a symmetry of that gear. No central gear is
    *     welded to another in a stacked compound, so one representative each.
    *  3. Planet k's body angle must satisfy its first mesh, which leaves exactly
    *     `Tp1` candidates per turn; all are tried against every other mesh.
    */
  def stacked(stages: Seq[Stage], n: Int, pinion: Pinion, x0: Rat = Rat.Zero): Boolean =
    require(stages.nonEmpty && stages.size <= 2 && n >= 2)
    val offsets: Seq[Rat] = pinion match
      case Pinion.Identical(g) => Seq(Rat.Zero, g).take(stages.size)
      case Pinion.Individual   => Seq.fill(stages.size)(Rat.Zero)
    val dir0 = Rat.Zero
    val centrals = stages.zip(offsets).map { (st, g) =>
      val h = x0 + g
      Centrals(st.sun.map(ts => sunFromPlanet(ts, st.planet, h, dir0)),
               ringFromPlanet(st.ring, st.planet, h, dir0))
    }
    // Sanity: planet 0 meshes everything by construction.
    assert(stages.indices.forall(i => halfMeshes(stages(i), centrals(i), x0 + offsets(i), dir0)))
    (1 until n).forall { k =>
      val dir = Rat.of(k, n)
      pinion match
        case Pinion.Individual =>
          stages.indices.forall { i =>
            candidates(stages(i), centrals(i), dir).exists(h => halfMeshes(stages(i), centrals(i), h, dir))
          }
        case Pinion.Identical(_) =>
          candidates(stages.head, centrals.head, dir).exists { x =>
            stages.indices.forall(i => halfMeshes(stages(i), centrals(i), x + offsets(i), dir))
          }
    }

  def single(st: Stage, n: Int): Boolean = stacked(Seq(st), n, Pinion.Individual)

  // ── Non-stacked compounds: two independent stages joined by central welds ──

  enum B:
    case S, R, C

  /** A rigid weld: stage-1 body `a` and stage-2 body `b`, with `angle(b) =
    * angle(a) + offset` (offset fixed when the part is made). */
  final case class Weld(a: B, b: B, offset: Rat)

  /** Place all planets of one stage given its central rotations and carrier
    * angle `phi`. Planets are single gears, so each is free to turn. */
  private def placeAll(st: Stage, n: Int, phi: Rat, c: Centrals): Boolean =
    (0 until n).forall { k =>
      val dir = phi + Rat.of(k, n)
      candidates(st, c, dir).exists(h => halfMeshes(st, c, h, dir))
    }

  /** Constructive search for a non-stacked compound (stage 1 with N1 planets,
    * stage 2 with N2, two welds into distinct stage-2 bodies). Returns true
    * only with a witness every mesh of which was checked by the predicates
    * above, so `true` is a proof of assemblability. (`false` can be trusted
    * where the per-stage condition fails, because then a stage cannot even be
    * assembled on its own: see `single`.) */
  def nonStacked(s1: Stage, n1: Int, s2: Stage, n2: Int, welds: Seq[Weld]): Boolean =
    require(welds.size == 2 && welds.map(_.b).distinct.size == 2)
    // Stage 1: carrier at 0 (global rotation), planet 0 at 0, centrals from planet 0.
    val c1 = Centrals(s1.sun.map(ts => sunFromPlanet(ts, s1.planet, Rat.Zero, Rat.Zero)),
                      ringFromPlanet(s1.ring, s1.planet, Rat.Zero, Rat.Zero))
    if !placeAll(s1, n1, Rat.Zero, c1) then return false
    def angle1(b: B): Option[Rat] = b match
      case B.S => c1.sun
      case B.R => Some(c1.ring)
      case B.C => Some(Rat.Zero)
    val known: Map[B, Rat] = welds.map { w =>
      val a = angle1(w.a).getOrElse(throw IllegalArgumentException("weld to an absent sun"))
      w.b -> (a + w.offset).frac
    }.toMap
    require(s2.sun.isDefined || !known.contains(B.S), "weld to an absent sun")
    (known.get(B.C), known.get(B.S), known.get(B.R)) match
      case (Some(phi), Some(sun), None) =>
        val ts = s2.sun.get
        planetsFromSun(ts, sun, s2.planet, phi).take(1).exists { h =>
          placeAll(s2, n2, phi, Centrals(Some(sun), ringFromPlanet(s2.ring, s2.planet, h, phi)))
        }
      case (Some(phi), None, Some(ring)) =>
        planetsFromRing(s2.ring, ring, s2.planet, phi).take(1).exists { h =>
          placeAll(s2, n2, phi, Centrals(s2.sun.map(ts => sunFromPlanet(ts, s2.planet, h, phi)), ring))
        }
      case (None, Some(sun), Some(ring)) =>
        // Carrier 2 is the unknown. Adding the two mesh conditions of planet 0
        // eliminates its rotation: (Ts+Tr)·φ ≡ Ts·σ + Tr·ρ − Tp/2. Every
        // candidate is re-checked by the mesh predicates in `placeAll`.
        val ts  = s2.sun.get
        val tot = ts + s2.ring
        val rhs = sun * ts + ring * s2.ring - Half * s2.planet
        (0 until tot).exists { j =>
          placeAll(s2, n2, (rhs + Rat(j)) / tot, Centrals(Some(sun), ring))
        }
      case other => throw IllegalArgumentException(s"unsupported weld pattern $other")
