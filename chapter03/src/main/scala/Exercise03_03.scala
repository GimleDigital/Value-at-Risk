package valueatrisk.chapter03

import scala.math.{pow, sqrt}

import breeze.linalg.DenseVector
import breeze.integrate.simpson

/** Proposed solution for exercise 3.3 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_03 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.3")

  // Exercise definitions
  val xMin = 1.0
  val xMax = 3.0

  def pf = (z: Double) => 0.5 * z

  // Precision in the approximations
  val iterations = 2000

  // 1. Obtaining E(Z)
  val expectedValue = simpson(pf, xMin, xMax, iterations)
  results.add(s"a) Expected value: ${utils.rounded(expectedValue, 5)}\n")

  // 2. Obtaining the variance
  val fv = (z: Double) => 0.5 * pow(z - expectedValue, 2)
  val variance = simpson(fv, xMin, xMax, iterations)
  results.add(s"b) Variance: ${utils.rounded(variance, 5)}\n")

  // 3. Obtaining the standard deviation
  val stdDev = sqrt(variance)
  results.add(s"c) Standard deviation: ${utils.rounded(stdDev, 5)}\n")

  // 4. Obtaining the skewness
  val fs = (z: Double) => 0.5 * pow(z - expectedValue, 3)
  val skewness = simpson(fs, xMin, xMax, iterations) / pow(stdDev, 3)
  results.add(s"d) Skewness: ${utils.rounded(skewness, 5)}\n")

  // 5. Obtaining the kurtosis
  val fk = (z: Double) => 0.5 * pow(z - expectedValue, 4)
  val kurtosis = simpson(fk, xMin, xMax, iterations) / pow(stdDev, 4)
  results.add(s"e) Kurtosis: ${utils.rounded(kurtosis, 5)}\n")

  // Writing the buffered results
  results.all('-', 80)
}
