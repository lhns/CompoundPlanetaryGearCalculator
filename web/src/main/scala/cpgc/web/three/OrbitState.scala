package cpgc.web.three

/** Camera orbit state for the 3D view (z up): spherical position about a pan-able target. Pure, no DOM. */
final class OrbitState(private var home: OrbitState.Home):
  import OrbitState.*

  private var yawV, pitchV, distV = 0.0
  private var tx, ty, tz          = 0.0
  reset()

  def yaw: Double      = yawV
  def pitch: Double    = pitchV
  def distance: Double = distV
  def target: (Double, Double, Double) = (tx, ty, tz)

  /** Adopt a new home (another design) keeping the user's angles, zoom ratio and pan offset. */
  def rehome(h: Home): Unit =
    val ratio = distV / home.distance
    val off   = (tx - home.target._1, ty - home.target._2, tz - home.target._3)
    home = h
    distV = h.distance * ratio
    tx = h.target._1 + off._1; ty = h.target._2 + off._2; tz = h.target._3 + off._3

  def reset(): Unit =
    yawV = DefaultYaw; pitchV = DefaultPitch; distV = home.distance
    tx = home.target._1; ty = home.target._2; tz = home.target._3

  /** Drag by (dx, dy) pixels: the model follows the pointer; dragging down raises the camera. */
  def orbit(dx: Double, dy: Double): Unit =
    yawV -= dx * RadiansPerPixel
    pitchV = clamp(pitchV + dy * RadiansPerPixel, MinPitch, MaxPitch)

  /** Wheel zoom; `deltaY > 0` moves away. */
  def zoom(deltaY: Double): Unit =
    distV = clamp(distV * math.exp(deltaY * 0.0015), home.distance * MinZoom, home.distance * MaxZoom)

  /** Multiplicative zoom (pinch), `factor < 1` moves closer. */
  def zoomBy(factor: Double): Unit =
    distV = clamp(distV * factor, home.distance * MinZoom, home.distance * MaxZoom)

  /** Pan by (dx, dy) pixels in a viewport `heightPx` tall with vertical field of view `fovRad`. */
  def pan(dx: Double, dy: Double, heightPx: Double, fovRad: Double): Unit =
    val perPixel = 2.0 * distV * math.tan(fovRad / 2.0) / math.max(1.0, heightPx)
    val (rx, ry, rz) = right
    val (ux, uy, uz) = up
    tx += (-dx * rx + dy * ux) * perPixel
    ty += (-dx * ry + dy * uy) * perPixel
    tz += (-dx * rz + dy * uz) * perPixel

  def position: (Double, Double, Double) =
    (tx + distV * math.cos(pitchV) * math.cos(yawV), ty + distV * math.cos(pitchV) * math.sin(yawV), tz + distV * math.sin(pitchV))

  /** Camera right and up vectors in world space. */
  def right: (Double, Double, Double) = (-math.sin(yawV), math.cos(yawV), 0.0)
  def up: (Double, Double, Double) =
    (-math.sin(pitchV) * math.cos(yawV), -math.sin(pitchV) * math.sin(yawV), math.cos(pitchV))

object OrbitState:
  /** Where reset() returns to: orbit target and distance. */
  final case class Home(target: (Double, Double, Double), distance: Double)

  val DefaultYaw: Double   = -math.Pi / 2.0 // camera on the -y side: x to the right, z up
  val DefaultPitch: Double = math.toRadians(32.0)
  val MinPitch: Double     = math.toRadians(-85.0)
  val MaxPitch: Double     = math.toRadians(85.0)
  val MinZoom              = 0.25
  val MaxZoom              = 4.0
  private val RadiansPerPixel = 0.008

  private def clamp(v: Double, lo: Double, hi: Double): Double = math.max(lo, math.min(hi, v))
