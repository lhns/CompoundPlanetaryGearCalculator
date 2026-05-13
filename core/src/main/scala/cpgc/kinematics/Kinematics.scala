package cpgc.kinematics

import cpgc.domain.{CompoundDesign, Mode}

/** Angles of every rotating body in a `CompoundDesign`, in radians, at a given
  * input phase. Values are full radians (not mod 2π) so consumers can interpolate
  * smoothly across revolutions.
  *
  *   - `sun1`, `sun2`: body rotation of the sun gears.
  *   - `carrier`: shared carrier angle.
  *   - `planet1Spin`, `planet2Spin`: world-frame body spin of each planet (not orbit).
  *   - `ring1`, `ring2`: body rotation of the ring gears (0 when grounded).
  */
final case class BodyAngles(
    sun1: Double,
    carrier: Double,
    planet1Spin: Double,
    ring1: Double,
    sun2: Double,
    planet2Spin: Double,
    ring2: Double
)

object Kinematics:

  /** Body angles as a function of the input phase φ (radians).
    *
    * For Standard/NSS, φ drives sun 1; for SingleStage, φ drives sun 1 (output is
    * the carrier); for CDCP, φ drives the carrier (input is the carrier).
    *
    * Derivation (teeth-units, ring 1 grounded for all non-CDCP modes):
    *
    *   sun-mesh:       (ω_sun − ω_c) · Ts = −(ω_p − ω_c) · Tp
    *   ring-mesh:       ω_r · Tr      =  ω_c · (Tr − Tp) + ω_p · Tp
    *
    *   ⇒ ω_c   = ω_sun · Ts / (Ts + Tr)
    *   ⇒ ω_p1  = −ω_c · (Tr1 − Tp1) / Tp1     (ring 1 fixed)
    *   ⇒ ω_r2  = (ω_c · (Tr2 − Tp2) + ω_p2 · Tp2) / Tr2
    *
    * Planets are rigidly coupled on a shared shaft: ω_p2 = ω_p1.
    */
  def at(d: CompoundDesign, phi: Double): BodyAngles = d.mode match
    case Mode.SingleStage =>
      val ts = d.stage1.set.sun.toDouble
      val tp = d.stage1.set.planet.toDouble
      val tr = d.stage1.set.ring.toDouble
      val sun1    = phi
      val carrier = phi * ts / (ts + tr)
      val pSpin   = -carrier * (tr - tp) / tp
      BodyAngles(sun1, carrier, pSpin, 0.0, 0.0, pSpin, 0.0)

    case Mode.Standard | Mode.NSS =>
      val ts1 = d.stage1.set.sun.toDouble
      val tp1 = d.stage1.set.planet.toDouble
      val tr1 = d.stage1.set.ring.toDouble
      val sun1    = phi
      val carrier = phi * ts1 / (ts1 + tr1)
      val pSpin   = -carrier * (tr1 - tp1) / tp1
      val (sun2, ring2) = d.stage2 match
        case None => (0.0, 0.0)
        case Some(st2) =>
          val ts2 = st2.set.sun.toDouble
          val tp2 = st2.set.planet.toDouble
          val tr2 = st2.set.ring.toDouble
          val ring2Angle = (carrier * (tr2 - tp2) + pSpin * tp2) / tr2
          val sun2Angle  =
            if d.mode == Mode.NSS then 0.0
            else carrier - (pSpin - carrier) * tp2 / ts2
          (sun2Angle, ring2Angle)
      BodyAngles(sun1, carrier, pSpin, 0.0, sun2, pSpin, ring2)

    case Mode.CDCP =>
      val carrier = phi
      val ts1 = d.stage1.set.sun.toDouble
      val tp1 = d.stage1.set.planet.toDouble
      val tr1 = d.stage1.set.ring.toDouble
      val pSpin = -carrier * (tr1 - tp1) / tp1
      // Sun 1 derived from sun mesh: (ω_s1 − ω_c)·Ts1 = −(ω_p − ω_c)·Tp1
      val sun1  = carrier + (carrier - pSpin) * tp1 / ts1
      val (sun2, ring2) = d.stage2 match
        case None => (0.0, 0.0)
        case Some(st2) =>
          val ts2 = st2.set.sun.toDouble
          val tp2 = st2.set.planet.toDouble
          val tr2 = st2.set.ring.toDouble
          val ring2Angle = (carrier * (tr2 - tp2) + pSpin * tp2) / tr2
          val sun2Angle  = carrier + (carrier - pSpin) * tp2 / ts2
          (sun2Angle, ring2Angle)
      BodyAngles(sun1, carrier, pSpin, 0.0, sun2, pSpin, ring2)

  /** Return the input and output body angles per the mode topology. */
  def io(d: CompoundDesign, b: BodyAngles): (Double, Double) = d.mode match
    case Mode.SingleStage          => (b.sun1, b.carrier)
    case Mode.Standard | Mode.NSS  => (b.sun1, b.ring2)
    case Mode.CDCP                 => (b.carrier, b.ring2)
