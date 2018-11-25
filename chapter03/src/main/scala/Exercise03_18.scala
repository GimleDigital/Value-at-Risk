package valueatrisk.chapter03

import scala.collection.mutable.ListBuffer

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 3.18 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_18 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.18")

  // Exercise definitions
  val uX = DenseMatrix(1.0, 0.0, -2.0).t

  val covX = DenseMatrix((1.0, 2.0, -1.0),(2.0, 13.0, 1.0),(-1.0, 1.0, 2.0))

  val uY = DenseMatrix(0.0, 0.0).t

  val covY = DenseMatrix.eye[Double](2)

  // Cholesky factorization

  val customCholesky = CustomCholesky.ImplCholesky_DM(covX)

  val c = customCholesky.map(utils.rounded(_, 5))

  results.add(s"Cholesky factorization:\n$c\n")

  // Removing extraneous columns of 0's

  val k = algebra.removeZeroColumns(customCholesky)

  results.add(s"k:\n$k\n")

  // Writing the buffered results
  results.all('-', 80)
}
