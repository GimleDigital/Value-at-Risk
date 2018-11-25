package valueatrisk.chapter03

import java.io.File

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 3.42 and 3.43 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_42_43 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.42 and 3.43")

  val means = csvread(new File("data/exercise-03-42-43-means.dat"), '	')
  val cov = csvread(new File("data/exercise-03-42-43-cov.dat"), '	')
  val c = csvread(new File("data/exercise-03-42-43-c.dat"), '	')
  val b = csvread(new File("data/exercise-03-42-43-b.dat"), '	')
  val a = csvread(new File("data/exercise-03-42-43-a.dat"), '	')

  // s1: Cholesky matrix
  val z = CustomCholesky.ImplCholesky_DM(cov)

  results.add(s"s1) Cholesky matrix:\n${z.map(utils.rounded(_, 4))}\n")

  // s2: Eigenvectors
  val u = algebra.eigenVectors(z.t * c * z).map(_ * -1)

  results.add(s"s2) u:\n${u.map(utils.rounded(_, 4))}\n")

  // s3 Change of variables
  val cc = u * z.t * c * z * u.t
  val bc = (2.0 * means.t * c + b) * z * u.t
  val ac = means.t * c * means + b * means + a

  results.add(s"s3) Change of variables\n")
  results.add(s"C:\n${cc.map(utils.rounded(_, 4))}\n")
  results.add(s"B: ${bc.map(utils.rounded(_, 4))}\n")
  results.add(s"A: ${ac.map(utils.rounded(_, 4))}\n")

  // 2.43: Calculating the mean and standard deviation
  results.add(s"2.43) Mean and standard deviation\n")

  val sCjj0 = diag(cc).toArray.reduce(_ + _)
  val g0 = means.t * c * means + b * means + a + sCjj0

  val g1 = 1 * sum(pow(bc, 2) * pow(diag(cc.map(_ * 2)), 0)) +
           0.5 * sum(pow(diag(cc).map(_ * 2), 2))

  results.add(
    s"g(0) = ${g0.map(utils.rounded(_, 4))}, g(1) = ${utils.rounded(g1, 4)}\n")

  val EY = g0
  val EY2 = g1 + g0(0, 0) * EY(0, 0)
  val stdDev = sqrt(EY2 - pow(EY, 2))

  results.add(s"E(Y) = $EY, E(Y2) = ${utils.rounded(EY2, 4)}\n")
  results.add(s"Standard deviation: ${stdDev.map(utils.rounded(_, 4))}\n")

  // Writing the buffered results
  results.all('-', 80)
}
