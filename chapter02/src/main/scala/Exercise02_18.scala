package valueatrisk.chapter02

import scala.math.exp

import breeze.linalg.DenseMatrix
import breeze.numerics.constants._

/** Proposed solution for exercise 2.18 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_18 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.18")

  /** Returns the value of function 1 in the exercise
   *
   *  @param x1 function variable 1
   *  @param x2 function variable 2
   */
  def f1(x1: Double, x2: Double) = {
    exp(x1 * x2)
  }

  /** Returns the value of function 2 in the exercise
   *
   *  @param x1 the value of function variable 1
   *  @param x2 the value of function variable 2
   */
  def f2(x1: Double, x2: Double) = {
    3 * x2
  }

  /** Returns the Jacobian matrix of the exercise, obtained analytically
   *
   *  @param x1 the value of function variable 1
   *  @param x2 the value of function variable 2
   */
   def analytical(x1: Double, x2: Double) = {
    val m11 = utils.rounded(exp(x1 * x2) * x2, 7)
    val m12 = utils.rounded(exp(x1 * x2) * x1, 7)
    val m21 = 0.0
    val m22 = 3.0

    DenseMatrix((m11, m12), (m21, m22))
  }

  /** Returns a Jacobian 2x2 matrix, obtained by central approximation
   *
   *  @param x1 the value of function variable 1
   *  @param x2 the value of function variable 2
   *  @param h the finite difference to use in the approximation
   */
  def centralApproximation(x1: Double, x2: Double, h: Double) = {
    val m11 = utils.rounded((f1(x1 + h/2, x2) - f1(x1 - h/2, x2))/h, 7)
    val m12 = utils.rounded((f1(x1, x2 + h/2) - f1(x1, x2 - h/2))/h, 7)
    val m21 = utils.rounded((f2(x1 + h/2, x2) - f2(x1 - h/2, x2))/h, 7)
    val m22 = utils.rounded((f2(x1, x2 + h/2) - f2(x1, x2 - h/2))/h, 7)

    DenseMatrix((m11, m12), (m21, m22))
  }

  /** Returns a Jacobian 2x2 matrix, obtained by forward approximation
   *
   *  @param x1 the value of function variable 1
   *  @param x2 the value of function variable 2
   *  @param h the finite difference to use in the approximation
   */
  def forwardApproximation(x1: Double, x2: Double, h: Double) = {
    val m11 = utils.rounded((f1(x1 + h, x2) - f1(x1, x2))/h, 7)
    val m12 = utils.rounded((f1(x1, x2 + h) - f1(x1, x2))/h, 7)
    val m21 = utils.rounded((f2(x1 + h, x2) - f2(x1, x2))/h, 7)
    val m22 = utils.rounded((f2(x1, x2 + h) - f2(x1, x2))/h, 7)

    DenseMatrix((m11, m12), (m21, m22))
  }

  // Obtaining the results
  val a = analytical (1.0, 1.0)
  results.add(s"Analytical:\n$a\n")

  val c = centralApproximation(1.0, 1.0, 0.00001)
  results.add(s"Central approximation:\n$c\n")

  val f = forwardApproximation(1.0, 1.0, 0.00001)
  results.add(s"Forward approximation:\n$f\n")

  // Writing the buffered results
  results.all('-', 80)
}