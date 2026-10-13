package cpgc.web.three

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** Minimal facades for the part of three.js r158 (global build, `window.THREE`) the 3D view uses. */
// Native constructor parameters exist only for the JS call (build.sbt silences the unused-parameter warning here).
object Three:

  @js.native @JSGlobal("THREE.Vector3")
  class Vector3() extends js.Object:
    var z: Double = js.native
    def set(x: Double, y: Double, z: Double): Vector3 = js.native

  @js.native @JSGlobal("THREE.Euler")
  class Euler() extends js.Object:
    var z: Double = js.native

  @js.native @JSGlobal("THREE.Object3D")
  class Object3D() extends js.Object:
    val position: Vector3 = js.native
    val rotation: Euler   = js.native
    val up: Vector3       = js.native
    def add(child: Object3D): Object3D    = js.native
    def remove(child: Object3D): Object3D = js.native
    def lookAt(x: Double, y: Double, z: Double): Unit = js.native

  @js.native @JSGlobal("THREE.Scene")
  class Scene() extends Object3D

  @js.native @JSGlobal("THREE.Group")
  class Group() extends Object3D

  @js.native @JSGlobal("THREE.AmbientLight")
  class AmbientLight(color: Int, intensity: Double) extends Object3D

  @js.native @JSGlobal("THREE.DirectionalLight")
  class DirectionalLight(color: Int, intensity: Double) extends Object3D

  @js.native @JSGlobal("THREE.PerspectiveCamera")
  class PerspectiveCamera(fov0: Double, aspect0: Double, near0: Double, far0: Double) extends Object3D:
    var aspect: Double = js.native
    var near: Double   = js.native
    var far: Double    = js.native
    def updateProjectionMatrix(): Unit = js.native

  @js.native @JSGlobal("THREE.BufferGeometry")
  class BufferGeometry() extends js.Object:
    def rotateX(angle: Double): BufferGeometry = js.native
    def dispose(): Unit = js.native

  @js.native @JSGlobal("THREE.Path")
  class Path() extends js.Object:
    def moveTo(x: Double, y: Double): Path = js.native
    def lineTo(x: Double, y: Double): Path = js.native
    def closePath(): Path = js.native

  @js.native @JSGlobal("THREE.Shape")
  class Shape() extends Path:
    val holes: js.Array[Path] = js.native
    def absarc(x: Double, y: Double, radius: Double, startAngle: Double, endAngle: Double, clockwise: Boolean): Shape = js.native

  @js.native @JSGlobal("THREE.ExtrudeGeometry")
  class ExtrudeGeometry(shape: Shape, options: js.Object) extends BufferGeometry

  @js.native @JSGlobal("THREE.CylinderGeometry")
  class CylinderGeometry(radiusTop: Double, radiusBottom: Double, height: Double, radialSegments: Int,
                         heightSegments: Int, openEnded: Boolean) extends BufferGeometry

  @js.native @JSGlobal("THREE.BoxGeometry")
  class BoxGeometry(width: Double, height: Double, depth: Double) extends BufferGeometry

  @js.native @JSGlobal("THREE.MeshStandardMaterial")
  class MeshStandardMaterial(parameters: js.Object) extends js.Object:
    def dispose(): Unit = js.native

  @js.native @JSGlobal("THREE.Mesh")
  class Mesh(geometry: BufferGeometry, material: MeshStandardMaterial) extends Object3D

  @js.native @JSGlobal("THREE.WebGLRenderer")
  class WebGLRenderer(parameters: js.Object) extends js.Object:
    val domElement: dom.HTMLCanvasElement = js.native
    def setSize(width: Double, height: Double, updateStyle: Boolean): Unit = js.native
    def setPixelRatio(ratio: Double): Unit = js.native
    def setClearColor(color: Int, alpha: Double): Unit = js.native
    def render(scene: Scene, camera: PerspectiveCamera): Unit = js.native
    def dispose(): Unit = js.native
    def forceContextLoss(): Unit = js.native

  /** True when the page can create a WebGL context (checked without touching three.js). */
  def webglAvailable(): Boolean =
    try
      val canvas = js.Dynamic.global.document.createElement("canvas")
      var gl = canvas.getContext("webgl2")
      if gl == null then gl = canvas.getContext("webgl")
      val ok = gl != null
      if ok then
        val ext = gl.getExtension("WEBGL_lose_context")
        if ext != null then ext.loseContext()
      ok && !js.isUndefined(js.Dynamic.global.THREE)
    catch case _: Throwable => false
