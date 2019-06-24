package valueatrisk.chapter10

import utils._

import java.io._

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 10.1 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise10_01 extends App {
  // Results are buffered and printed later
  val results = new Results("10.1")

  // Exercise definitions
  val s0 = DenseMatrix(30.0, 45.0, 60.0, 20.0)
  val w = DenseMatrix(100.0, 250.0, -200.0, 500.0)
  val sigma = csvread(new File("data/exercise-10-01-sample.dat"), '	')

  // b) Current and estimaded portfolio values
  val p0 = w.t * s0
  val eP1 = p0

  results.add("b) Current and estimated portfolio values")
  results.add(s"P0: ${p0}, E(P0): ${eP1}")

  // c) Standard deviation of portfolio value
  val stdDev = sqrt(w.t * sigma * w)

  results.add("c) Standard deviation of portfolio value")
  results.add(s"${stdDev.map(doubleToString(_, 2))}")

  // d) Portfolio Value-at-Risk
  val VaR = sqrt(2.0) * erfinv(2 * 0.95 - 1) * stdDev

  results.add("d) Portfolio Value-at-Risk")
  results.add(s"${VaR.map(doubleToString(_, 2))}")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
