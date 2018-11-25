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

/** Proposed solution for exercise 4.19 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise04_19 extends JFXApp {
  // Exercise definitions
  val whiteNoise = csvread(new File("data/exercise-04-18-19-20-wn.dat"), '	')

  def autoRegression(x1: Double, x2: Double, w: Double): Double =
    2.0 + 0.70 * x1 - 0.10 * x2 + w

  // Calculating the values of xt
  val xt = new ArrayBuffer[(Int, Double)]

  xt.append((0, autoRegression(0.0, 0.0, whiteNoise(0, 0))))
  xt.append((1, autoRegression(xt(0)._2, 0.0, whiteNoise(1, 0))))

  for (i <- 2 to whiteNoise.rows - 1)
    xt.append((i, autoRegression(xt(i - 1)._2, xt(i - 2)._2, whiteNoise(i, 0))))

  // Defining the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 4.19"

    // Displaying the graphical user interface
    scene = new Scene {
      stylesheets = List(getClass.getResource("styles.css").toExternalForm)

      val timeSeriesChart = new TimeSeriesChart("Time", "Moving Average")

      root = new VBox {
        styleClass.add("exercise-4-18-19-20")

        children.addAll(timeSeriesChart.getContent(xt.toSeq))
      }
    }
  }
}
