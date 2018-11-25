package valueatrisk.chapter03

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 3.14 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_14 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.14")

  // Z = (A + B)
  val b = DenseVector(1.0, 1.0).t

  // var(A) = var(B) = 9, 0.25 = cov(A, B) / 9
  val variances = DenseMatrix((9.0, 9.0 / 4.0), (9.0 / 4.0, 9.0))

  val stdDev = sqrt(b * variances * b.t)

  val s = utils.rounded(stdDev, 3)
  results.add(s"Standard deviation: $s\n")

  // Writing the buffered results
  results.all('-', 80)
}
