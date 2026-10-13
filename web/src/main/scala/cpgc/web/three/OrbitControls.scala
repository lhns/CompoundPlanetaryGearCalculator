package cpgc.web.three

import org.scalajs.dom
import scala.scalajs.js

/** Mouse / touch input for the 3D view: drag orbits, shift- or right-drag pans, wheel and pinch zoom,
  * double-click resets. Drives an `OrbitState`; `onChange` asks the view to redraw. */
final class OrbitControls(
    canvas: dom.HTMLCanvasElement, orbit: OrbitState, fovRad: Double, onChange: () => Unit
):
  private var pointers = Map.empty[Int, (Double, Double)]
  private var panning  = false

  private val onDown: js.Function1[dom.PointerEvent, Unit] = e =>
    pointers = pointers.updated(e.pointerId.toInt, (e.clientX, e.clientY))
    panning = e.button == 2 || e.shiftKey
    try canvas.asInstanceOf[js.Dynamic].setPointerCapture(e.pointerId) catch case _: Throwable => ()

  private val onMove: js.Function1[dom.PointerEvent, Unit] = e =>
    val id = e.pointerId.toInt
    pointers.get(id).foreach { (px, py) =>
      val (dx, dy) = (e.clientX - px, e.clientY - py)
      if pointers.size >= 2 then
        val before = spread()
        pointers = pointers.updated(id, (e.clientX, e.clientY))
        val after = spread()
        if after > 1.0 && before > 1.0 then orbit.zoomBy(before / after)
      else
        pointers = pointers.updated(id, (e.clientX, e.clientY))
        if panning then orbit.pan(dx, dy, canvas.clientHeight.toDouble, fovRad) else orbit.orbit(dx, dy)
      onChange()
    }

  private val onUp: js.Function1[dom.PointerEvent, Unit] = e =>
    pointers = pointers - e.pointerId.toInt

  private val onWheel: js.Function1[dom.WheelEvent, Unit] = e =>
    e.preventDefault()
    orbit.zoom(if e.deltaMode == 1 then e.deltaY * 16.0 else e.deltaY)
    onChange()

  private val onDouble: js.Function1[dom.MouseEvent, Unit] = _ =>
    orbit.reset()
    onChange()

  private val onContext: js.Function1[dom.Event, Unit] = e => e.preventDefault()

  private def spread(): Double =
    val ps = pointers.values.toVector
    if ps.size < 2 then 0.0 else math.hypot(ps(0)._1 - ps(1)._1, ps(0)._2 - ps(1)._2)

  canvas.style.setProperty("touch-action", "none")
  canvas.addEventListener("pointerdown", onDown)
  canvas.addEventListener("pointermove", onMove)
  canvas.addEventListener("pointerup", onUp)
  canvas.addEventListener("pointercancel", onUp)
  canvas.addEventListener("wheel", onWheel)
  canvas.addEventListener("dblclick", onDouble)
  canvas.addEventListener("contextmenu", onContext)

  def detach(): Unit =
    canvas.removeEventListener("pointerdown", onDown)
    canvas.removeEventListener("pointermove", onMove)
    canvas.removeEventListener("pointerup", onUp)
    canvas.removeEventListener("pointercancel", onUp)
    canvas.removeEventListener("wheel", onWheel)
    canvas.removeEventListener("dblclick", onDouble)
    canvas.removeEventListener("contextmenu", onContext)
