package valueatrisk.chapter02

import breeze.linalg._
import breeze.linalg.functions._

/** Proposed solution for exercise 2.7 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise02_07 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.7")

  val c = DenseMatrix((3.0, 1.0), (2.0, 2.0))
  val eigenValues = eig(c).eigenvalues

  val e1 = eigenValues(0)
  val e2 = eigenValues(1)

  results.add(s"Eigenvalues: $e1, $e2\n")

  // The eigenvectors can be found with the following conditions
  val I = DenseMatrix.eye[Double](2)

  val m1 = c - e1 * I
  results.add("The first eigenvector must fullfill the conditions:")
  results.add(s"${m1(0,0)} x1 + ${m1(0,1)} x2 = 0 and")
  results.add(s"${m1(1,0)} x1 + ${m1(1,1)} x2 = 0\n")

  val m2 = c - e2 * I
  results.add("The second eigenvector must fullfill the conditions:")
  results.add(s"${m2(0,0)} x1 + ${m2(0,1)} x2 = 0 and")
  results.add(s"${m2(1,0)} x1 + ${m2(1,1)} x2 = 0\n")

  // Writing the buffered results
  results.all('-', 80)
}