package cpgc.web

import com.raquo.laminar.api.L.*
import cpgc.domain.CompoundDesign
import cpgc.kinematics.Kinematics
import cpgc.web.Layout3D.Tone
import cpgc.web.UiKit.ariaLabel
import cpgc.web.three.*
import cpgc.web.three.Three.*
import org.scalajs.dom
import scala.scalajs.js

/** The 3D view: a three.js canvas with orbit controls. The WebGL renderer exists only while this
  * element is mounted. Meshes are rebuilt when the design changes; per frame only rotations change. */
object ThreeView:

  private val FovDeg = 35.0

  // Part colours; stage tints match the 2D schematic. Input / output / fixed read the page's CSS accents.
  private val BaseColors: Map[Tone, Int] = Map(
    Tone.Sun1 -> 0xffd166, Tone.Ring1 -> 0x118ab2, Tone.Planet1 -> 0x06d6a0,
    Tone.Sun2 -> 0xff9f1c, Tone.Ring2 -> 0x9d4edd, Tone.Planet2 -> 0x73d2de,
    Tone.Stacked -> 0x06d6a0, Tone.Carrier -> 0x8b949e, Tone.Weld -> 0xf0883e,
    Tone.Fixed -> 0x4b5563, Tone.Input -> 0x7ee787, Tone.Output -> 0x58a6ff
  )

  /** `#rrggbb` value of a CSS custom property, if it has that form. */
  private def cssColor(name: String): Option[Int] =
    try
      val v = dom.window.getComputedStyle(dom.document.documentElement).getPropertyValue(name).trim
      Option.when(v.matches("#[0-9a-fA-F]{6}"))(Integer.parseInt(v.drop(1), 16))
    catch case _: Throwable => None

  private def palette(): Tone => Int =
    val roles = Map(Tone.Input -> "--role-input", Tone.Output -> "--role-output")
    val all   = BaseColors ++ roles.flatMap((t, v) => cssColor(v).map(t -> _))
    all

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
        camera.aspect = w / h
        camera.updateProjectionMatrix()
        draw()

    def clear(): Unit =
      current.foreach { (_, s) => scene.remove(s.root); s.dispose() }
      current = None

    /** Rebuild for a new design (or empty); returns whether the design has a visible weld. */
    def show(d: Option[CompoundDesign]): Boolean =
      clear()
      val welded = d.exists { design =>
        val layout = Layout3D.build(design)
        val s      = new ThreeScene(layout, palette())
        scene.add(s.root)
        current = Some((design, s))
        val home = OrbitState.Home(
          (0.0, 0.0, (layout.zMin + layout.zMax) / 2.0),
          layout.outerRadius * 2.6 * 35.0 / FovDeg)
        orbit.rehome(home)
        pose(phase)
        layout.hasWeld
      }
      draw()
      welded

    def pose(p: Double): Unit =
      phase = p
      current.foreach((d, s) => s.pose(Kinematics.at(d, p)))
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
    val hasWeld  = Var(false)
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
        swatch("var(--role-input)", "input"), swatch("var(--role-output)", "output"),
        swatch("#4b5563", "fixed"),
        child.maybe <-- hasWeld.signal.map(Option.when(_)(swatch("#f0883e", "weld / coupling")))),
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
            val controls = new OrbitControls(canvas, () => live.orbit, math.toRadians(FovDeg), () => live.draw())
            val observer = new dom.ResizeObserver((_, _) => live.resize(hostRef.clientWidth.toDouble, hostRef.clientHeight.toDouble))
            live.cleanup = () => { observer.disconnect(); controls.detach() }
            observer.observe(hostRef)
            live.resize(hostRef.clientWidth.toDouble, hostRef.clientHeight.toDouble)

            design.foreach(d => hasWeld.set(live.show(d)))(ctx.owner)
            phase.foreach(live.pose)(ctx.owner)
            Some(live),
        unmount = (_, live) => live.flatten.foreach(_.dispose())
      )
    )
