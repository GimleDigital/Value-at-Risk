package valueatrisk.chapter14

import utils._

import scala.collection.mutable.ArrayBuffer

import java.io._

import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.{TableCell, TableColumn, TableView}
import scalafx.scene.layout.{GridPane}

import breeze.linalg._

/** Proposed solution for exercise 14.8 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise14_08 extends JFXApp {
  // Exercise definitions
  val sample = csvread(new File("data/exercise-14-08-sample.dat"), ';')

  // a) Exceedence count
  def exceedences(count: Int, iter: Int): Int = {
    if (iter < 0) count
    else {
      if (sample(iter, 0) < -1.0 * sample(iter, 1)) exceedences(count + 1, iter - 1)
      else exceedences(count, iter - 1)
    }
  }

  val exc = exceedences(0, sample.rows - 1)

  val parameterValuesA = Array(Array(exc.toString))

  val headerA = Array("Exceedences")
  val tableA = new Table(headerA, parameterValuesA)

  // b) Graphical backtest
  val combinedChart = new CombinedChart("Time", "EUR (millions)")

  val line = new ArrayBuffer[(Int, Double)]
  val plot = new ArrayBuffer[(Int, Double)]

  for (i <- 0 to sample.rows - 1) {
    line.append((i + 1, sample(i, 0) * -1.0)) // VaR is loss, hence negative
    plot.append((i + 1, sample(i, 1)))
  }

  val chartB = combinedChart.getChart(line, plot)

  // c) Standard coverage test
  val alpha = sample.rows - 1
  val q = 0.99
  val epsilon = 0.05

  val standardCoverage = new StandardCoverage(alpha + 1, q, epsilon)

  val x = standardCoverage.getX

  val xValues = new ArrayBuffer[(Int, Int)]

  xValues.append((max(0, x(0)), x(1)))
  xValues.append((x(0) + 1, x(1)))
  xValues.append((max(0, x(0)), x(1) - 1))
  xValues.append((x(0) + 1, x(1) - 1))

  val trials = standardCoverage.getTrials

  def findMax(i: Int, maxP: Double, j: Int): Int = {
    if (i > 3) j
    else {
      if (trials(i) > maxP && trials(i) <= epsilon) findMax(i + 1, trials(i), i)
      else findMax(i + 1, maxP, j)
    }
  }

  val k = findMax(0, 0.0, 0)

  val resultC = if(xValues(k)._1 <= exc && exc <= xValues(k)._2) "Accepted"
    else "Rejected"

  val parameterValuesC = Array(Array(xValues(k)._1.toString,
    xValues(k)._2.toString, doubleToString(trials(k), 4),
    doubleToString(epsilon, 2), resultC))

  val headerC = Array("x1", "x2", "Probability", "Epsilon", "Result")
  val tableC = new Table(headerC, parameterValuesC)

  // d) Kupiec’s PF coverage test
  val kupiec = new Kupiec(alpha, q, epsilon)

  val solutions = kupiec.getSolutions(0, Array(0, 0))

  val resultD = if(solutions(0) <= exc && exc <= solutions(1)) "Accepted"
    else "Rejected"


  val parameterValuesD = Array(Array(solutions(0).toString,
    solutions(1).toString, resultD))

  val headerD = Array("x1", "x2", "Result")
  val tableD = new Table(headerD, parameterValuesD)

  // Displaying the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 14.8"

    def getHeader(str: String) = new Label {
      styleClass.add("header")

      text = str
    }

    scene = new Scene {
      stylesheets = List(getClass.getResource("styles.css").toExternalForm)

      root = new GridPane

      val gridPane = new GridPane {
        styleClass.add("gridpane")
      }

      gridPane.add(getHeader("a) Exceedence count"), 0, 0)
      gridPane.add(tableA.getTable, 0, 1)
      gridPane.add(getHeader("b) Graphical backtest"), 1, 0)
      gridPane.add(chartB, 1, 1)
      gridPane.add(getHeader("c) Standard coverage test"), 2, 0)
      gridPane.add(tableC.getTable, 2, 1)
      gridPane.add(getHeader("d) Kupiec’s PF coverage test"), 3, 0)
      gridPane.add(tableD.getTable, 3, 1)

      content.add(gridPane)
    }
  }
}
