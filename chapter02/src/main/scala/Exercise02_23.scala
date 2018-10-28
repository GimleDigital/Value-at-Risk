package valueatrisk.chapter02

import scala.math.pow

import breeze.linalg.{det, DenseMatrix}
import breeze.integrate._

/** Proposed solution for exercise 2.23 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_23 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.23")

  // Min and max values
  val xMin = -1.0
  val xMax = 0.0

  def innerMin(x: Double) = 2 * x + 2
  def innerMax(x: Double) = x + 2

  // Number of iterations
  val iter = 2000

  // Evaluating the original integral by using Simpson's rule
  def originalFunction(x1: Double, x2: Double) =
    pow(x2, 2) - x1 * x2 - x1 + 2 * x2 + 1

  def originalOuterFunction = (x2: Double) =>
    (pow(x2, 3) + 3 * pow(x2, 2) + 2 * x2) / 2

  def originalOuterIntegral =
    simpson(originalOuterFunction, xMin, xMax, iter)

  results.add(s"Original integral: = $originalOuterIntegral\n")

  // Evaluating the integral after change of variables

  val uMin = 0.0
  val uMax = 1.0

  // The jacobian that is used for changing variables
  def jacobian = DenseMatrix((1, 2), (1, 0))
  val determinant = det(jacobian)

  results.add(s"Jacobian of f(u):\n$jacobian\n")
  results.add(s"Determinant: $determinant\n")

  // Resolving
  def changedOuterFunction = (u1: Double) => (pow(u1, 3) - u1) / 2

  def changedOuterIntegral = simpson(changedOuterFunction, uMin, uMax, iter)

  results.add(s"Changed integral: = $changedOuterIntegral\n")

  // Writing the buffered results
  results.all('-', 80)
}
