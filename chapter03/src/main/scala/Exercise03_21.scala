package valueatrisk.chapter03

import java.io.File

import breeze.linalg._, eigSym.EigSym
import breeze.numerics._

/** Proposed solution for exercise 3.21 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_21 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.21")

  val covariances = csvread(new File("data/exercise-03-21-cov.dat"), '	')
  val means = csvread(new File("data/exercise-03-21-means.dat"), '	')

  // a) determinant of correlation matrix
  val correlations = algebra.covToCorr(covariances)
  val determinant = det(correlations)

  results.add(s"1. Determinant: ${utils.rounded(determinant, 6)}\n")

  // b) Multicollinearity
  val criterion = 0.01 // Criterion for multicollinearity
  val str = if (determinant > criterion) " not" else ""
  results.add(s"2. Z is$str multicollinear\n")

  // c) Eigenvalues and eigenvectors
  val eigenValues = reverse(eigSym(covariances).eigenvalues)

  val eigenVectors = algebra.reverseCols(eigSym(covariances).eigenvectors)

  results.add(s"3. Eigenvalues: ${eigenValues.map(utils.rounded(_, 4))}\n")
  results.add(s"   Eigenvectors:\n${eigenVectors.map(utils.rounded(_, 4))}\n")

  results.add(s"(Equivalent solution: multiply 2nd column by scalar -1)\n")

  // e) Covariance matrix of the vector of principal components
  val d = diag(eigenValues)

  results.add(
    s"5. Covariances of principal components:\n${d.map(utils.rounded(_, 4))}\n")

  // f) Covariance matrix for D
  val n = eigenVectors.cols
  val covD = diag(eigenValues(0 to n - 2))

  results.add(s"6. Covariances of D\n${covD.map(utils.rounded(_ , 4))}\n")

  // g) Approximation of Z
  val vVector = eigenVectors.delete(n - 1, Axis._1)

  results.add(s"7. Approximation of Z\n")
  results.add(s"Vector v:\n${vVector.map(utils.rounded(_ , 4))}\n")
  results.add(s"Vector u:\n${means.map(utils.rounded(_ , 4))}\n")

  // Writing the buffered results
  results.all('-', 80)
}
