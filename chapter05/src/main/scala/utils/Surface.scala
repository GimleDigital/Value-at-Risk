package valueatrisk.chapter05

import scala.collection.mutable.ArrayBuffer
import scala.math.{max, min}

import scalafx.scene.image.PixelWriter
import scalafx.scene.image.WritableImage
import scalafx.scene.paint.Color
import scalafx.scene.paint.PhongMaterial
import scalafx.scene.DepthTest
import scalafx.scene.shape.CullFace
import scalafx.scene.shape.DrawMode
import scalafx.scene.shape.MeshView
import scalafx.scene.shape.TriangleMesh

/** Draws a three-dimensional surfaces, representing y values calculated as a
 *  function of x and z values. The surface is painted with a blend of two
 *  colors, according to the hight of each y value.
 *
 *  The code is based on an example found at https://stackoverflow.com/
 *  questions/31073007/how-to-create-a-3d-surface-chart-with-javafx and has been
 *  simplified and ported to ScalaFx.
 *
 * @param f the function y = f(x, z)
 * @param x the values of x
 * @param y the values of y
 * @param amplitude the amplitude in pixels for the y values
 * @param color1 the base color of the surface
 * @param color2 the secondary color of the surface
 */
case class Surface(f: (Double, Double) => Float, xValues: Array[Double],
                   zValues: Array[Double], amplitude: Int, color1: Color,
                   color2: Color) {
  val xSize = xValues.length
  val zSize = zValues.length

  val xLength = xSize.toFloat
  val zLength = zSize.toFloat

  // Used for normalizing the y values
  var yMax: Float = 0

  val yValues = Array.ofDim[Float](xSize, zSize)

  for (x <- 0 to xSize - 1) {
    for (z <- 0 to zSize - 1) {
      yValues(x)(z) = (f(xValues(x), zValues(z))).toFloat
      if (yValues(x)(z) > yMax) yMax = yValues(x)(z)
    }
  }

  // Stores surface contours
  val triangleMesh = new TriangleMesh

  // Stores surface colors
  val writableImage = new WritableImage(xSize, zSize)
  val pw = writableImage.pixelWriter

  for (x <- 0 to xSize - 1) {
    for (z <- 0 to zSize - 1) {
      val  y = 1 - yValues(x)(z) / yMax

      triangleMesh.points.addAll(x, y * amplitude, z)

      val color = color2.interpolate(color1, y)

      pw.setColor(x, z, color)
    }
  }

  // Texture and faces coordinates
  for (x <- 0 to xSize - 2) {
    for (z <- 0 to zSize - 2) {
      val x0: Float = x / xLength
      val z0: Float = z / zLength
      val x1: Float = (x + 1) / xLength
      val z1: Float = (z + 1) / zLength

      triangleMesh.texCoords.addAll( //
        x0, z0, // 0, top-left
        x0, z1, // 1, bottom-left
        x1, z1, // 2, top-right
        x1, z1 // 3, bottom-right
      )

      val topLeft = x * xSize + z
      val bottomLeft = x * xSize + z + 1
      val topRight = (x + 1) * xSize + z
      val bottomRight = (x + 1) * xSize + z + 1

      val offset = (x * (xSize - 1) + z ) * 8 / 2

      triangleMesh.faces.addAll(bottomLeft, offset + 1, topLeft, offset + 0,
                                topRight, offset + 2)
      triangleMesh.faces.addAll(topRight, offset + 2, bottomRight, offset + 3,
                                bottomLeft, offset + 1)
    }
  }

  // Coloring the surface
  val phongMaterial = new PhongMaterial {
    diffuseMap = writableImage
    specularColor = Color.White
  }

  // Returns the complete surface
  def getMeshView = new MeshView(triangleMesh) {
    translateX = -0.5 * xSize
    translateY = -0.5 * xSize
    translateZ = -0.5 * zSize
    material = phongMaterial
    cullFace = CullFace.Back
    drawMode = DrawMode.Fill
    depthTest = DepthTest.Enable
  }
}