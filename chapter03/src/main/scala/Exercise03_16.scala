package valueatrisk.chapter03

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 3.16 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise03_16 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.16")

  // Exercise definitions
  val b = DenseMatrix(2.0, -3.0).t

  val covariances = DenseMatrix((25.0, 0.00), (0.0, 16.0))

  // Reformulation of the problem
  val identityMatrix = DenseMatrix.eye[Double](b.cols)

  val expanded = DenseMatrix.vertcat(identityMatrix, b)

  results.add(s"Expanded matrix:\n$expanded\n")

  // Expanded variance matrix
  val variances = expanded * covariances * expanded.t

  results.add(s"Variance matrix:\n$variances\n")

  // Correlation between X1 and X3
  val correlation13 = variances(0, 2) / (sqrt(variances(0,0))
                      * sqrt(variances(2,2)))

  val c = utils.rounded(correlation13, 3)
  results.add(s"Correlation(X1, X3): $c\n")

  // Writing the buffered results
  results.all('-', 80)
}
