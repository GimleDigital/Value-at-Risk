package valueatrisk.chapter05

import scalafx.scene.Group
import scalafx.scene.paint.Color
import scalafx.scene.transform.Rotate

/** Draws three sides of a cube.
 *
 *  TODO: For some reason, the cube has to be drawn as front, bottom and left
 *  sides and then rotated 180 degrees in order to display correctly another
 *  graphical object inside the cube.
 *
 *  The code is based on an example found at https://stackoverflow.com/
 *  questions/31073007/how-to-create-a-3d-surface-chart-with-javafx and has been
 *  simplified and ported to ScalaFx.
 *
 *  @param size the size in pixels
 *  @param background the color of the cube sides
 *  @param gridColor the color of the grid
 */
case class Cube(size: Int, background: Color, gridColor: Color) extends Group {
  // The back of the cube (drawn as front, c.f. TODO above)
  val back = new Axis(size,
                      background.deriveColor(0.0, 1.0, (1 - 0.5 * 1), 1.0),
                      gridColor) {
    translateX = -0.5 * size
    translateY = -0.5 * size
    translateZ = -0.5 * size
  }

  // The back of the cube
  val bottom = new Axis(size,
                        background.deriveColor(0.0, 1.0, (1 - 0.4 * 1), 1.0),
                        gridColor) {
    translateX = -0.5 * size
    translateY = 0
    rotationAxis = Rotate.XAxis
    rotate = 90
  }


  // The left side of the cube (drawn as right side, c.f. TODO above)
  val left = new Axis(size,
                      background.deriveColor(0.0, 1.0, (1 - 0.2 * 1), 1.0),
                      gridColor) {
    translateX = -1.0 * size
    translateY = -0.5 * size
    rotationAxis = Rotate.YAxis
    rotate = 90
  }

  children.addAll(back, bottom, left)
}
