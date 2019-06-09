package valueatrisk.chapter09

import utils._

import java.io.File

import breeze.linalg._
import breeze.numerics.{sqrt}

/** Proposed solution for exercise 9.8 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise09_08 extends App {
  // Results are buffered and printed later
  val results = new Results("9.8")

  // Exercise definitions
  val u = DenseMatrix(8.5, 7.0, 3.0, -4.0, -3.0, 0.0, -10.0, -14.0)

  val sigma = csvread(new File("data/exercise-09-08-sample.dat"), '	')

  val stdDevs = diag(diag(sqrt(sigma)).map(1 / _))

  val determinant = det(stdDevs * sigma * stdDevs)

  // a) Determinant of the correlation matrix
  results.add("a) Determinant of the correlation matrix:")
  results.add(s"${doubleToString(determinant, 6)}")

  // c) Principal components remapping
  val v = eig(sigma).eigenvectors
  val d = eig(sigma).eigenvalues

  val vr = v(0 to 7, 0 to 5)
  val dr = DenseMatrix(d(0 to 5)).t

  val rSpreads = vr * dr + DenseMatrix(u(::, 0)).t

  results.add("c) Principal components remapping")
  results.add("Six first Eigenvectors:")
  results.add(s"${vr.map(doubleToString(_, 3))}")
  results.add("Six first principal components:")
  results.add(s"${dr.t.map(doubleToString(_, 3))}")
  results.add("Spreads:")
  results.add(s"${rSpreads.t.map(doubleToString(_, 3))}")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
