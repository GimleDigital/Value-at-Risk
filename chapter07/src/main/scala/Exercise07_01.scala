package valueatrisk.chapter07

import utils._

import scala.collection.mutable.ArrayBuffer

import java.io._

import breeze.linalg._

/** Proposed solution for exercise 7.1 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise07_01 extends App {
  // Results are buffered and printed later
  val results = new Results("7.1")

  // Exercise definition
  val dataMatrix = csvread(new File("data/exercise-07-01-sample.dat"), '	')

  // White noise data
  val b = new ArrayBuffer[DenseMatrix[Double]]
  for (m <- 0 to dataMatrix.cols - 1) {
    val item = DenseMatrix.eye[Double](dataMatrix.rows)

    for (n <- 1 to dataMatrix.rows - 1) item(n, n) = 1.0 / dataMatrix(n - 1, m)

    b.append(item)
  }

  val a = DenseMatrix.fill(dataMatrix.rows, 1)(1.0)

  val R = DenseMatrix.zeros[Double](dataMatrix.rows, dataMatrix.cols)

  for (m <- 0 to R.cols - 1) {
    R(0 to R.rows - 1, m to m) := b(m) * DenseMatrix(dataMatrix(::, m)).t - a
  }

  R(0 to 0, 0 to R.cols - 1) := 9999.0 // Dummy values, first row has no data

  results.add("White Noise Data")
  results.add(R.map(doubleToString(_, 4)).toString)

  // White noise covariances
  val Terms = new ArrayBuffer[DenseMatrix[Double]]

  for (n <- 1 to R.rows - 1) {
    val item = R(n, 0 to R.cols - 1)

    Terms.append(item.t * item)
  }

  val S = DenseMatrix.zeros[Double](R.cols, R.cols)

  for (n <- 0 to S.cols -1) {
    for (m <- 0 to S.rows - 1) {
      var value = 0.0
      for (i <- 0 to Terms.length - 1) {
        value += Terms(i)(n, m)
      }
      S(n, m) = value / 9.0
    }
  }

  results.add("White Noice Covariances")
  results.add(S.map(doubleToString(_, 7)).toString)

  // Aluminium price covariances
  val b1 = DenseMatrix.eye[Double](S.cols)

  for (i <- 0 to b1.cols - 1) b1(i, i) = 1.0 / dataMatrix(dataMatrix.rows - 1, i)

  val Cov = inv(b1) * S * inv(b1)

  results.add("Aluminium Price Covariances")
  results.add(Cov.map(doubleToString(_, 1)).toString)

  // Printing the results
  results.all('-', 80)

  Thread.sleep(1000)
}
