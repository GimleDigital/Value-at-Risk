package valueatrisk.chapter14

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.sqrt

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
import breeze.numerics.erfinv
import breeze.stats.distributions.Gaussian

/** Proposed solution for exercise 14.9 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise14_09 extends JFXApp {
  // Exercise definitions
  val sample = csvread(new File("data/exercise-14-09-sample.dat"), ';')

  val gaussian = new Gaussian(0, 1)

  /** Returns the quantile of a normal distribution for a given probability.
   *
   *  @param mu the distribution's mean
   *  @param sigma the distribution's standard deviation
   *  @param p the probability
   */
  def inverseCdf(mu: Double, sigma: Double, p: Double): Double = {
    mu + sigma * sqrt(2) * erfinv(2.0 * p - 1.0)
  }

  // a) Loss quantile data
  val factor99 = sqrt(2) * erfinv(2 * 0.99 - 1) // Approximately 2.326

  val conditionalStdDev = sample(::, 0).map(_ / factor99)

  val pl = sample(::, 1)

  val tU = (pl.map(-1.0 * _) :/ conditionalStdDev).map(gaussian.cdf(_))

  // b) Loss quantiles
  val tN = tU.map(inverseCdf(0.0, 1.0, _)).toArray

  // c) Ordering loss quantiles
  scala.util.Sorting.quickSort(tN)

  // d) Reference quantiles
  val nJn = new ArrayBuffer[Double]

  for (j <- 1 to tN.length) nJn.append(inverseCdf(0.0, 1.0, (j - 0.5) /
    tN.length))

  // e) Plotting the quantiles
  val chartData = new ArrayBuffer[(Double, Double)]

  for (i <- 0 to tN.length - 1) chartData.append((tN(i), nJn(i)))

  val varScatterChart = new VARScatterChart("n(j)", "n*(j)")

  val chartE = varScatterChart.getChart(chartData)

  // f) Sample correlation
  val correlation = new Correlation(DenseVector(tN), DenseVector(nJn.toArray))

  val corrCoef = correlation.coefficient
  val nonReject = 0.986 // significance 0.05, sample size 125

  val resultF = if (corrCoef < nonReject) "Rejected" else "Accepted"

  val parameterValuesF = Array(Array(doubleToString(corrCoef, 3),
    doubleToString(nonReject, 3), resultF))

  val headerF = Array("Correlation", "Threshold", "Result")
  val tableF = new Table(headerF, parameterValuesF)

  // Displaying the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 14.9"

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

      gridPane.add(getHeader("e) Plotting the quantiles"), 0, 0)
      gridPane.add(chartE, 0, 1)
      gridPane.add(getHeader("f) Sample correlation"), 1, 0)
      gridPane.add(tableF.getTable, 1, 1)

      content.add(gridPane)
    }
  }
}
