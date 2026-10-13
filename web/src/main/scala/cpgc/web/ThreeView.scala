package cpgc.web

import com.raquo.laminar.api.L.*
import cpgc.constraints.Coupling
import cpgc.domain.CompoundDesign
import cpgc.kinematics.Kinematics
import cpgc.web.Palette.Tone
import cpgc.web.UiKit.ariaLabel
import cpgc.web.three.*
import cpgc.web.three.Three.*
import org.scalajs.dom
import scala.scalajs.js

/** The 3D view: a three.js canvas with orbit controls. The WebGL renderer exists only while this
  * element is mounted. Meshes are rebuilt when the design changes; per frame only rotations change. */
object ThreeView:

  private val FovDeg = 35.0

  private val FovRad = math.toRadians(FovDeg)

  private def light: Boolean =
    try dom.window.matchMedia("(prefers-color-scheme: light)").matches catch case _: Throwable => false

  private def color(t: Tone): Int = Palette.hex(t, light)
  private def css(t: Tone): String = Palette.hexString(color(t))

  private def message(state: SearchState, found: Int): String = state match
    case SearchState.NoResults => "No design matches these constraints."
    case SearchState.Invalid   => "Fix the highlighted input to search."
    case _                     => if found > 0 then s"Searching designs… ($found found)" else "Searching designs…"

  /** "Schematic | 3D" segmented toggle for the panel header. */
  def toggle(view: Var[View]): HtmlElement =
    def seg(v: View, name: String) =
      button(cls := "seg", typ := "button", name,
        cls("active") <-- view.signal.map(_ == v),
        aria.pressed <-- view.signal.map(x => (x == v).toString),
        onClick.preventDefault.mapTo(v) --> view)
    div(cls := "seg-group", role := "group", ariaLabel := "View", seg(View.Schematic, "Schematic"), seg(View.ThreeD, "3D"))

  private def swatch(css: String, name: String): HtmlElement =
    span(cls := "three-legend-item",
      span(cls := "three-swatch", styleAttr := s"background:$css"), name)

  private final class Live(renderer: WebGLRenderer, scene: Scene, camera: PerspectiveCamera):
    var current: Option[(CompoundDesign, ThreeScene)] = None
    private var layout: Option[Scene3D.Layout] = None
    private var aspect = 1.0
    val orbit = new OrbitState(OrbitState.Home((0.0, 0.0, 0.0), 300.0))
    var cleanup: () => Unit = () => ()
    var phase = 0.0

    def draw(): Unit =
      val (x, y, z)    = orbit.position
      val (tx, ty, tz) = orbit.target
      camera.position.set(x, y, z)
      camera.lookAt(tx, ty, tz)
      renderer.render(scene, camera)

    def resize(w: Double, h: Double): Unit =
      if w > 0 && h > 0 then
        renderer.setSize(w, h, false)
        aspect = w / h
        camera.aspect = aspect
        camera.updateProjectionMatrix()
        layout.foreach(l => orbit.rehome(home(l))) // keeps the user's zoom; an untouched view stays fitted
        draw()

    def clear(): Unit =
      current.foreach { (_, s) => scene.remove(s.root); s.dispose() }
      current = None
      layout = None

    /** Orbit home that frames the whole model at the current canvas aspect. */
    private def home(l: Scene3D.Layout): OrbitState.Home =
      OrbitState.Home((0.0, 0.0, (l.zMin + l.zMax) / 2.0), Scene3D.fitDistance(Scene3D.boundingRadius(l), FovRad, aspect))

    /** Rebuild for a new design (or empty); returns its couplings for the legend. */
    def show(d: Option[CompoundDesign]): Vector[(Int, Coupling)] =
      clear()
      val cs = d.fold(Vector.empty[(Int, Coupling)]) { design =>
        val model  = GearboxScene.of(design)
        val layout = Scene3D.build(model)
        val s      = new ThreeScene(layout, color)
        scene.add(s.root)
        current = Some((design, s))
        this.layout = Some(layout)
        orbit.refit(home(layout))
        applyPhase()
        model.couplings
      }
      draw()
      cs

    private def applyPhase(): Unit = current.foreach((d, s) => s.pose(Kinematics.at(d, phase)))

    def pose(p: Double): Unit =
      phase = p
      applyPhase()
      draw()

    def dispose(): Unit =
      clear()
      cleanup()
      renderer.dispose()
      renderer.forceContextLoss()
      renderer.domElement.remove()

  def apply(
      design: Signal[Option[CompoundDesign]],
      phase: Signal[Double],
      searchState: Signal[SearchState],
      progress: Signal[Int]
  ): HtmlElement =
    val noWebgl  = Var(false)
    val couplings = Var(Vector.empty[(Int, Coupling)])
    val host     = div(cls := "three-host")
    val hostRef  = host.ref

    div(
      cls := "three-view",
      host,
      div(cls := "three-msg",
        child.text <-- noWebgl.signal.combineWith(design, searchState, progress).map {
          case (true, _, _, _)             => "3D view needs WebGL, which is not available in this browser. Use the schematic view."
          case (_, Some(_), _, _)          => ""
          case (_, None, st, n)            => message(st, n)
        }),
      div(cls := "three-legend",
        Palette.RoleLegend.map((tone, name) => swatch(css(tone), name)),
        children <-- couplings.signal.map { cs =>
          cs.map((slot, c) => swatch(css(Palette.slotTone(slot)), SchematicLayout.couplingLegendLabel(c)))
        }),
      onMountUnmountCallbackWithState[HtmlElement, Option[Live]](
        mount = ctx =>
          if !Three.webglAvailable() then
            noWebgl.set(true)
            None
          else
            val renderer = new WebGLRenderer(js.Dynamic.literal(antialias = true, alpha = true))
            renderer.setPixelRatio(math.min(dom.window.devicePixelRatio, 2.0))
            renderer.setClearColor(0x000000, 0.0) // transparent: the panel's themed background shows through
            val canvas = renderer.domElement
            canvas.style.display = "block"
            canvas.style.width = "100%"
            canvas.style.height = "100%"
            hostRef.appendChild(canvas)

            val scene  = new Scene()
            val camera = new PerspectiveCamera(FovDeg, 1.0, 0.1, 5000.0)
            camera.up.set(0, 0, 1)
            scene.add(new AmbientLight(0xffffff, 0.75))
            val key = new DirectionalLight(0xffffff, 1.6); key.position.set(120, -200, 300); scene.add(key)
            val fill = new DirectionalLight(0xffffff, 0.7); fill.position.set(-200, 150, -100); scene.add(fill)

            val live     = new Live(renderer, scene, camera)
            val controls = new OrbitControls(canvas, live.orbit, FovRad, () => live.draw())
            val observer = new dom.ResizeObserver((_, _) => live.resize(hostRef.clientWidth.toDouble, hostRef.clientHeight.toDouble))
            live.cleanup = () => { observer.disconnect(); controls.detach() }
            observer.observe(hostRef)
            live.resize(hostRef.clientWidth.toDouble, hostRef.clientHeight.toDouble)

            design.foreach(d => couplings.set(live.show(d)))(ctx.owner)
            phase.foreach(live.pose)(ctx.owner)
            Some(live),
        unmount = (_, live) => live.flatten.foreach(_.dispose())
      )
    )
