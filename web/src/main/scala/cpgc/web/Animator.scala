package cpgc.web

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** rAF-driven animation clock.
  *
  *  - `phase` is a continuously increasing input-shaft angle in radians.
  *  - `running` toggles motion (advance halts when false but the rAF loop keeps polling).
  *  - `speed` is revolutions/second of the input body — driven by a UI slider.
  */
final class Animator:
  val running: Var[Boolean] = Var(true)
  val speed:   Var[Double]  = Var(0.4)

  private val phaseVar: Var[Double] = Var(0.0)
  val phase: Signal[Double] = phaseVar.signal

  private var lastT: Double = -1.0

  private val step: scala.scalajs.js.Function1[Double, Unit] = { (t: Double) =>
    if running.now() then
      if lastT < 0 then lastT = t
      val dt = (t - lastT) / 1000.0
      lastT = t
      phaseVar.update(_ + 2.0 * math.Pi * speed.now() * dt)
    else
      lastT = -1.0
    dom.window.requestAnimationFrame(step)
    ()
  }

  def start(): Unit =
    dom.window.requestAnimationFrame(step)
    ()
