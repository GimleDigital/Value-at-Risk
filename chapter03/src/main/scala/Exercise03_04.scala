package valueatrisk.chapter03

import scala.math.{pow, sqrt}

import breeze.linalg.DenseVector
import breeze.integrate.simpson

/** Proposed solution for exercise 3.4 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_04 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.4")

  // Exercise definitions
  val xMin = 1.0
  val xMax = 3.0

  def pf = (z: Double) => 0.5 * pow(z, 2)

  // Precision in the approximations
  val iterations = 2000

  // 1. Obtaining E(Z)
  val expectedValue = simpson(pf, xMin, xMax, iterations)
  results.add(s"a) Expected value: ${utils.rounded(expectedValue, 5)}\n")

  // 2. Obtaining the variance
  val fv = (z: Double) => 0.5 * pow(pow(z, 2) - expectedValue, 2)
  val variance = simpson(fv, xMin, xMax, iterations)
  results.add(s"b) Variance: ${utils.rounded(variance, 5)}\n")

  // 3. Obtaining the standard deviation
  val stdDev = sqrt(variance)
  results.add(s"c) Standard deviation: ${utils.rounded(stdDev, 5)}\n")

  // Writing the buffered results
  results.all('-', 80)
}
