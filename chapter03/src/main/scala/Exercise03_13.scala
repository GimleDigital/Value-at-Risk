package valueatrisk.chapter03

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 3.13 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise03_13 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.13")

  // Exercise definitions
  val a = 10.0
  val b = DenseVector(1.0, 3.0, -2.0).t

  val means = DenseVector(-4.0, 0.0, 5.0)
  val stdDevs = DenseVector(1.1, 0.7, 0.4)
  val correlations = DenseMatrix((1.0, 0.3, 0.1), (0.3, 1.0, -0.2),
                                 (0.1, -0.2, 1.0))

  // Obtaining the mean
  val mean = b * means + a

  results.add(s"E(Y): ${utils.rounded(mean, 5)}\n")

  // Obtaining the standard deviation
  val variances = diag(stdDevs) * correlations * diag(stdDevs)

  val v = variances.map(utils.rounded(_, 3))
  results.add(s"Variance matrix:\n$v\n")

  val stdDev = sqrt(b * variances * b.t)

  val s = utils.rounded(stdDev, 3)
  results.add(s"Standard deviation: $s\n")

  // Writing the buffered results
  results.all('-', 80)
}
