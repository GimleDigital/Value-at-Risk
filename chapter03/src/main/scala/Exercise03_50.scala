package valueatrisk.chapter03

import scala.collection.mutable.ArrayBuffer

import java.io.File

import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants._

/** Proposed solution for exercise 3.50 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_50 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.50")

  // Exercise definitions
  val alpha = -7.0
  val beta = 6.0
  val gamma = Array(4.0, 3.0)
  val variance = Array(0.0, 4.0)

  // Approximation of cumulative density function
  val u = 1.0 // Maximum value for trapezoid evaluation
  val n = 500 // Trapezoid steps
  val y0 = 0.0 // First seed value
  val y1 = 1.0 // Second seed value
  val q = 0.10 // Quantile

  val nccs = new NonCentralChiSquare(alpha, beta, gamma, variance)

  val cdf0 = nccs.cdf(u, n, y0)
  val cdf1 = nccs.cdf(u, n, y1)

  results.add(s"Phi(0): ${utils.rounded(cdf0, 5)}")
  results.add(s"Phi(1): ${utils.rounded(cdf1, 5)}")

  val estimatedY = nccs.pValue(q, u, n, y0, y1, 1)

  results.add(s"0.10 quantile of Y: ${utils.rounded(estimatedY, 3)}")

  // Writing the buffered results
  results.all('-', 80)
}
