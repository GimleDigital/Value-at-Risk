package valueatrisk

import scala.collection.mutable.ListBuffer

import java.io._

import breeze.linalg._
import breeze.numerics._

/** Proposed solutions for exercise 1.12 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise01_12 extends App {
  // Uploading tab separated data from files.
  val weights = csvread(new File("data/exercise-01-12-weights.dat"),'	')
  val covariances =
    csvread(new File("data/exercise-01-12-covariances.dat"),'	')

  // Accumulated value of 1 USD in each instrument
  val securities = (DenseVector.ones[Double](7))

  // Results buffer, to be written later.
  val results = new utils.Results("1.12")

  // b) Using the formula p = w·s, value p at t = 0
  val portfolioValue = weights * securities
  results.addResult(s"b) Initial portfolio value: ${portfolioValue(0)}\n")

  // e) Determine the conditional standard deviation
  val variance = (weights * covariances * weights.t)
  val stdDev = sqrt(variance)
  results.addResult(s"e) Standard deviation: $stdDev\n")

  // f) Calculate the .10-quantile of price at t = 1
  val initialPrice = weights.toArray.fold(0.0)(_ + _)
  val quantile10  = initialPrice - 1.282 * stdDev
  results.addResult(s"f) 10% quantile: $quantile10\n")

  // g) Calculate the portfolio’s 1-year 90% USD value-at-risk
  val oneYear90percentUSDVar = initialPrice - quantile10
  results.addResult(s"g) 1 Year 90% USD VaR: $oneYear90percentUSDVar\n")

  // Writing the buffererd results
  results.printResults('-')
}
