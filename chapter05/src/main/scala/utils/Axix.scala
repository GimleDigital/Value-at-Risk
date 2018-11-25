package valueatrisk.chapter05

import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line
import scalafx.scene.shape.Rectangle

/** Draws a square with a grid.
 *
 *  The code is based on an example found at https://stackoverflow.com/
 *  questions/31073007/how-to-create-a-3d-surface-chart-with-javafx and has been
 *  simplified and ported to ScalaFx.
 *
 *  @param size the size in pixels
 *  @param sideColor the color of the square
 *  @param gridColor the color of the grid
 */
case class Axis(size: Int, sideColor: Color, gridColor: Color) extends Pane {
  // Drawing the rectangle
  val rectangle = new Rectangle {
    width = size
    height = size
    fill = sideColor
  }

  children.add(rectangle)

  // Drawing the horizontal lines of the grid
  for (y <- 0 to 10) {
    val line = new Line {
      endX = size.toDouble
      stroke = gridColor
      fill = gridColor
      translateY = y * size / 10
    }

    children.add(line)
  }

  // Drawing the vertial lines of the grid
  for (x <- 0 to 10) {
    val line = new Line {
      endY = size.toDouble
      stroke = gridColor
      fill = gridColor
      translateX = x * size / 10
    }

    children.add(line)
  }
}
