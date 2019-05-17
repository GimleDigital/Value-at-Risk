package valueatrisk.chapter07

import utils._

import scala.collection.mutable.ArrayBuffer

import java.io._

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 7.3 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise07_03 extends App {
  // Results are buffered and printed later
  val results = new Results("7.3")

  // Exercise definition
  val covariances1 = csvread(new File("data/exercise-07-03-sample.dat"), '	')

  // Obtaining eigenvalues of original matrix
  val eig1 = DenseMatrix(eig(covariances1).eigenvalues)

  results.add(
    s"Original matrix' eigenvalues: ${eig1.map(doubleToString(_, 4))toString}")

  // Obtaining eigenvalues of modified matrix
  val epsilon = 0.0005
  val mod = diag(DenseVector.fill(covariances1.rows){epsilon})

  val covariances2 = covariances1 + mod

  val eig2 = DenseMatrix(eig(covariances2).eigenvalues)

  results.add(
    s"Modified matrix' eigenvalues: ${eig2.map(doubleToString(_, 4))toString}")

  // Printing the results
  results.all('-', 80)

  Thread.sleep(1000)
}
