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

/** Proposed solution for exercise 4.20 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise04_20 extends JFXApp {
  // Exercise definitions
  val whiteNoise = csvread(new File("data/exercise-04-18-19-20-wn.dat"), '	')


  def arMa(x1: Double, w0: Double, w1: Double): Double =
    2.0 + w0 - 0.25 * w1 + 0.50 * x1

  // Calculating the autoRegressive x values
  val xt = new ArrayBuffer[(Int, Double)]

  xt.append((0, 0.0))

  for (i <- 1 to whiteNoise.rows - 1)
    xt.append((i, arMa(xt(i - 1)._2, whiteNoise(i, 0), whiteNoise(i - 1, 0))))

  // Defining the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 4.20"

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
