package valueatrisk.chapter03

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 3.40 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_40 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.40")

  // Exercise definitions
  val means = DenseMatrix(-1.0, 1.0)
  val variances = DenseMatrix(4.0, 9.0)
  val weights = DenseMatrix(0.3, 0.7)

  // Mean
  val mean = weights.t * means

  results.add(s"1) Mean: ${utils.rounded(mean(0, 0), 4)}\n")

  // Standard deviation
  val stdDev = sqrt(weights.t * (variances + pow(means, 2)) - pow(mean, 2))

  results.add(s"2) Standard deviation: ${utils.rounded(stdDev(0, 0), 4)}\n")

  // .25-quantile
  def normal1 = new statistics.NormalDistribution(means(0,0), variances(0,0))
  def normal2 = new statistics.NormalDistribution(means(1,0), variances(1,0))

  // Initial values for the newton method
  val seedValue = DenseMatrix(0.3, 0.1)

  // Returns the value of the evaluated functions for a given vector of x.
  def function = (m: DenseMatrix[Double]) =>
    DenseMatrix(0.3 * m(0,0) + 0.7 * m(1,0) - 0.25,
    normal1.inverseCdf(m(0,0)) - normal2.inverseCdf(m(1,0)))

  // Returns the values in the Jacobian matrix for a given vector of x.
  def jacobian = (m: DenseMatrix[Double]) => DenseMatrix((0.3, 0.7),
    (normal1.inverseCdfDiff(m(0,0)), - 1 * normal2.inverseCdfDiff(m(1,0))))

  val probabilities = algebra.newton(function, jacobian, seedValue)

  val p = probabilities.map(utils.rounded(_, 4))
  results.add(s"3) .25-quantile\n\nprobabilities: ${p(0, 0)}, ${p(1, 0)}\n")

  val quantile = normal1.inverseCdf(p(0,0)) // = normal2.inverseCdf(p(1,0))

  results.add(s".25-quantile: ${utils.rounded(quantile, 4)}\n")

  // Writing the buffered results
  results.all('-', 80)
}
