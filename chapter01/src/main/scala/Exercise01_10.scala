package valueatrisk.chapter01

import scala.collection.mutable.ArrayBuffer
import scala.math.sqrt

import java.util.Locale

import scalafx.application.JFXApp
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.paint.Color._
import scalafx.scene.layout.VBox

import breeze.linalg.{DenseVector}
import breeze.stats.distributions.Binomial

/** Proposed solution for exercise 1.12 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise01_10 extends JFXApp {
  val n = 20
  val p = 0.9

  val portfolioValues = DenseVector(1 to n toArray) // Expressed in USD 1000s
  val binomial = new Binomial(n, p)

  // Results are buffered, to be written later on
  val values = new ArrayBuffer[String]
  val results = new utils.Results("1.10")

  // Calculating the probability for all feasible portfolio values
  val probabilities = new ArrayBuffer[Number]

  for (k <- 1 to n) {
    val probability = utils.getRounded(binomial.probabilityOf(k), 5)
    val q = probability
    probabilities += q

    val str = "%f".formatLocal(java.util.Locale.US, probability)
    values += s"$str if 1p = $k\n"
  }

  // Calculating standard deviation for the binomial distribution
  val stdDev = utils.getRounded(sqrt(binomial.variance) * 1000.0, 2)

  // The numerical results are written to console
  results.addResult(values.reverse.fold("")(_ + _))
  results.addResult(s"Standard deviation for 1P = $stdDev")

  results.printResults('-', 80)

  // Creating a GUI and displaying the graph
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 1.10"
    width = 768
    height = 512
    scene = new Scene {
      fill = PALEGOLDENROD
      content = new VBox {
        spacing = 10
        padding = Insets(30)

        val distributionChart = new DistributionChart(
          "Probability function", "Portfolio Value (USD 1000s)", "Probability"
        )

        val barChart = distributionChart.getChart(
          portfolioValues.toArray, probabilities.toSeq
        )

        val resultLabel = new Label(
          s"Standard deviation for the portfolio value = $stdDev"
        )

        children.addAll(barChart, resultLabel)
      }
    }
  }
}