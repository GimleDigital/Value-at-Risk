package valueatrisk.chapter03

import scala.collection.mutable.ArrayBuffer

import java.io.File

import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants._

/** Proposed solution for exercise 3.50 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_50 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.50")

  // Exercise definitions
  val alpha = DenseMatrix(7.0)
  val beta = DenseMatrix(6.0)
  val gammas = DenseMatrix(4.0, 3.0, 0.0)
  val variances = DenseMatrix(1.0, 1.0, 1.0)

  // Sum expressions in the partials of the transformed CDF formula
  def sumA(w: Double) = {
    val buffer = new ArrayBuffer[Double]

    for (i <- 0 to gammas.rows - 1) {
      buffer.append(pow(gammas(i, 0), 2) * variances(i, 0) /
                    (1.0 + 4.0 * pow(gammas(i, 0), 2) * pow(w, 2)))
    }

    buffer.reduce(_ + _)
  }
  def sumB(w: Double) = {
    val buffer = new ArrayBuffer[Double]

    for (i <- 0 to gammas.rows - 1) {
      buffer.append(gammas(i, 0) * variances(i, 0) /
                    (1.0 + 4.0 * pow(gammas(i, 0), 2) * pow(w, 2)))
    }

    buffer.reduce(_ + _)
  }
  def sumC(w: Double) = {
    val buffer = new ArrayBuffer[Double]

    for (i <- 0 to gammas.rows - 1) buffer.append(atan(2 * gammas(i, 0) * w))

    buffer.reduce(_ + _)
  }
  def productD(w: Double) = {
    val buffer = new ArrayBuffer[Double]

    for (i <- 0 to gammas.rows - 1)
      buffer.append(1.0 + 4.0 * pow(gammas(i, 0), 2) * pow(w, 2))

    buffer.reduce(_ * _)
  }

  // Partials of the transformed CDF formula
  def partA(w: Double) =
    0.5 * pow(w, 2) * (pow(beta, 2) + 4.0 * sumA(w))
  def partB(w: Double, y: Double) = w * (alpha - y + sumB(w))
  def partC(w: Double) = 0.5 * sumC(w)
  def partD(w: Double) = w * pow(productD(w), 0.25)

  // Transformed CDF formula at w = 0
  def tCdf0(y: Double) = alpha - y + gammas.t * (variances + 1.0)

  // Transformed CDF formula at w = 1
  def integralPart = (w: Double) => {
    val m = (exp(partA(w)) * sin(partB(w, 1.0) + partC(w))) / partD(w)

    m(0, 0)
  }
  def trapezoid = algebra.trapezoidalIntegral(integralPart, 0.0, 1.0, 500)
  def tCdf(w: Double, y: Double) = 0.5 - (1.0 / Pi) * trapezoid

  results.add(s"tCdf0: ${tCdf0(0)}\n")
  results.add(s"tCdf(1, 1): ${tCdf(1, 1)}\n")

  /** ToDo: These values are not correct, the formulas have to be revised
   *  before completing the rest of the exercise.
   */
  results.add(
    s"ERROR: These values are not correct, the formulas have to be revised.\n")

  // Writing the buffered results
  results.all('-', 80)
}
