package valueatrisk.chapter07

import utils._

import java.io._

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 7.2 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise07_02 extends App {
  // Results are buffered and printed later
  val results = new Results("7.2")

  // Exercise definition
  val dataMatrix = csvread(new File("data/exercise-07-02-sample.dat"), '	')

  // White Noise Data
  val b = DenseMatrix.eye[Double](dataMatrix.rows)
  for (n <- 1 to dataMatrix.rows - 1) b(n, n) = 1.0 / dataMatrix(n - 1, 0)

  val a = DenseMatrix.fill(dataMatrix.rows, 1)(1.0)

  val R = b.t * dataMatrix - a

  R(0, 0) = 9999.0 // Dummy value, first row has no data

  results.add("a) White Noise Data")
  results.add(R.map(doubleToString(_, 4)).toString)

  // b) UWMA standard deviations obtained with sample estimator 7.10
  val assumedMean1 = 0.0
  val wnStdDev1 = sqrt(R(1 to R.rows - 1, 0).map(pow(_, 2)).sum / (R.rows - 1))
  val clStdDev1 = wnStdDev1 * dataMatrix(dataMatrix.rows - 1,  0)

  results.add("b) UWMA standard deviations obtained with sample estimator 7.10")
  results.add(s"- Assumed mean: ${doubleToString(assumedMean1, 4)}")
  results.add(s"- Estimated for white noise: ${doubleToString(wnStdDev1, 4)}")
  results.add(s"- Estimated for CHF Libor: ${doubleToString(clStdDev1, 4)}")

  // c) UWMA Standard deviations (obtained with sample estimator 7.9)
  val assumedMean2 = R(1 to R.rows - 1, 0).sum /(R.rows - 1)

  val R2 = R.map(_ - assumedMean2)

  val wnStdDev2 = sqrt(R2(1 to R2.rows - 1, 0).map(pow(_, 2)).sum /
    (R2.rows - 1))
  val clStdDev2 = wnStdDev2 * dataMatrix(dataMatrix.rows - 1,  0)

  results.add("c) UWMA standard deviations obtained with sample estimator 7.9")
  results.add(s"- Assumed mean: ${doubleToString(assumedMean2, 4)}")
  results.add(s"- Estimated for white noise: ${doubleToString(wnStdDev2, 4)}")
  results.add(s"- Estimated for CHF Libor: ${doubleToString(clStdDev2, 4)}")

  // b) EWMA standard deviations
  val lambda = 0.95

  val assumedMean3 = 0.0

  val weights = DenseMatrix((0 to dataMatrix.rows - 2).toArray.reverse)
    .map(pow(lambda, _))

  val R3 = weights :* R(1 to R.rows - 1, 0).map(pow(_, 2)).t

  val wnStdDev3 = sqrt(R3.sum * (1.0 - lambda) / (1.0 - pow(lambda, R3.cols)))
  val clStdDev3 = wnStdDev3 * dataMatrix(dataMatrix.rows - 1,  0)

  results.add("d) EWMA standard deviations obtained with sample estimator 7.20")
  results.add(s"- Assumed mean: ${doubleToString(assumedMean3, 4)}")
  results.add(s"- Estimated for white noise: ${doubleToString(wnStdDev3, 4)}")
  results.add(s"- Estimated for CHF Libor: ${doubleToString(clStdDev3, 4)}")

  // Printing the results
  results.all('-', 80)

  Thread.sleep(1000)
}
