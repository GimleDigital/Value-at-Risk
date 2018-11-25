package valueatrisk.chapter04

import scala.collection.mutable.ArrayBuffer

import java.io.File

import scalafx.application.JFXApp
import scalafx.collections.ObservableBuffer
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.layout.VBox


import breeze.linalg._

/** Proposed solution for exercise 4.18 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise04_18 extends JFXApp {
  // Exercise definitions
  val whiteNoise = csvread(new File("data/exercise-04-18-19-20-wn.dat"), '	')

  def movingAverage(w: DenseVector[Double]): Double =
    -2.0 + w(2) + 0.6 * w(1) + 0.3 * w(0)

  // Calculating the moving averages
  val xt = new ArrayBuffer[(Int, Double)]

  val v = whiteNoise(::, 0)

  for (i <- 0 to whiteNoise.rows - 3) xt += ((i, movingAverage(v(i to i + 2))))

  // Defining the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 4.18"

    // Displaying the graphical user interface
    scene = new Scene {
      stylesheets = List(getClass.getResource("styles.css").toExternalForm)

      val timeSeriesChart = new TimeSeriesChart("Time", "Moving Averages")

      root = new VBox {
        styleClass.add("exercise-4-18-19-20")

        children.addAll(timeSeriesChart.getContent(xt.toSeq))
      }
    }
  }
}
