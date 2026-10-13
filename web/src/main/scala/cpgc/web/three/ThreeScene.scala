package cpgc.web.three

import cpgc.kinematics.BodyAngles
import cpgc.web.Palette.Tone
import cpgc.web.{GearboxScene, Scene3D}
import cpgc.web.Scene3D.{Bar, Cyl, Extruded, Prim}
import cpgc.web.three.Three.*
import scala.collection.mutable
import scala.scalajs.js

/** The meshes of one design, built from its `Scene3D` layout. Built once per design; `pose` only sets rotations. */
final class ThreeScene(layout: Scene3D.Layout, color: Tone => Int):

  val root: Group = new Group()

  private val geometries = mutable.ArrayBuffer.empty[BufferGeometry]
  private val materials  = mutable.Map.empty[Int, MeshStandardMaterial]
  private val clusterGroups = mutable.ArrayBuffer.empty[(Scene3D.Cluster, Group)]
  private val planetGroups  = mutable.ArrayBuffer.empty[(Scene3D.PlanetPart, Group)]

  private def material(tone: Tone): MeshStandardMaterial =
    val c = color(tone)
    materials.getOrElseUpdate(c, new MeshStandardMaterial(
      js.Dynamic.literal(color = c, metalness = 0.25, roughness = 0.55, side = 2 /* DoubleSide */)))

  private def mesh(g: BufferGeometry, tone: Tone): Mesh =
    geometries += g
    new Mesh(g, material(tone))

  private def toMesh(p: Prim): Mesh = p match
    case e @ Extruded(gear, z0, z1) =>
      val (outline, outerRadius, tone) = (e.outline, gear.outerRadius, e.tone)
      val shape = new Shape()
      def trace(path: Path): Unit =
        path.moveTo(outline.head._1, outline.head._2)
        outline.tail.foreach((x, y) => path.lineTo(x, y))
        path.closePath()
      outerRadius match
        case Some(r) =>
          shape.absarc(0, 0, r, 0, 2.0 * math.Pi, false)
          val hole = new Path()
          trace(hole)
          shape.holes.push(hole)
        case None => trace(shape)
      val m = mesh(new ExtrudeGeometry(shape, js.Dynamic.literal(depth = z1 - z0, bevelEnabled = false, curveSegments = 96)), tone)
      m.position.z = z0
      m
    case Cyl(x, y, rTop, rBottom, z0, z1, open, tone) =>
      val m = mesh(new CylinderGeometry(rTop, rBottom, z1 - z0, 32, 1, open).rotateX(math.Pi / 2.0), tone)
      m.position.set(x, y, (z0 + z1) / 2.0)
      m
    case Bar(x0, y0, x1, y1, width, z0, z1, tone) =>
      val m = mesh(new BoxGeometry(math.hypot(x1 - x0, y1 - y0), width, z1 - z0), tone)
      m.position.set((x0 + x1) / 2.0, (y0 + y1) / 2.0, (z0 + z1) / 2.0)
      m.rotation.z = math.atan2(y1 - y0, x1 - x0)
      m

  for c <- layout.clusters do
    val g = new Group()
    c.prims.foreach(p => g.add(toMesh(p)))
    for pl <- c.planets do
      val pg = new Group()
      pg.position.set(pl.x, pl.y, 0)
      pl.prims.foreach(p => pg.add(toMesh(p)))
      g.add(pg)
      planetGroups += ((pl, pg))
    root.add(g)
    clusterGroups += ((c, g))

  /** Set every rotation from the body angles. The scene's angles are clockwise on screen (y down); about
    * the 3D z axis (y up) that is the opposite sense, hence the one sign flip. */
  def pose(a: BodyAngles): Unit =
    clusterGroups.foreach((c, g) => g.rotation.z = -GearboxScene.clusterAngle(c.bodies, a))
    planetGroups.foreach((p, g) => g.rotation.z = -GearboxScene.angle(p.gear, a))

  def dispose(): Unit =
    geometries.foreach(_.dispose())
    materials.values.foreach(_.dispose())
    geometries.clear(); materials.clear(); clusterGroups.clear(); planetGroups.clear()
