package valueatrisk.chapter04

import scala.collection.mutable.ArrayBuffer

import java.io.File

import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants._

/** Proposed solution for exercise 4.5 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise04_05 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("4.5")

  // Maximum number of iterations for the Newton-Raphson method
  val maxIterations = 1000

  // Initial values for the Newton-Raphson method
  val seedValues1 = DenseMatrix(0.4, 0.5)
  val seedValues2 = DenseMatrix(-1.3, 1.8)

  // Exercise definitions
  val x = csvread(new File("data/exercise-04-05-sample.dat"), '	')

  /** Returns the first value of the log-likelihood function's gradient for some
   *  trial values of means u at a given value of x.
   *
   *  @param xk the given value of x
   *  @param u the trial values for u
   */
  def gradient1(xk: Double, u: DenseMatrix[Double]) =
    ((xk - u(0, 0)) * exp(pow(xk - u(0, 0), 2) / -2.0) ) /
    ((exp(pow(xk - u(0, 0), 2) / -2.0) + exp(pow(xk - u(1, 0), 2) / -2.0)) )

  /** Returns the second value of the log-likelihood function's gradient for
   *  some trial values of means u at a given value of x.
   *
   *  @param xk the given value of x
   *  @param u the trial values for u
   */
  def gradient2(xk: Double, u: DenseMatrix[Double]) =
    ((xk - u(1, 0)) * exp(pow(xk - u(1, 0), 2) / -2.0) / (8.0 * Pi)) /
    ((exp(pow(xk - u(0, 0), 2) / -2.0) + exp(pow(xk - u(1, 0), 2) / -2.0)) )

  /** Returns the sum of the values of the log-likelihood function's gradients
   *  for some trial values of means u at all values of x in the sample.
   *
   *  @param u the trial values of means u
   */
  def function = (u: DenseMatrix[Double]) =>
    DenseMatrix(sum(x.map(gradient1(_, u))),
                sum(x.map(gradient2(_, u))))

  /** Returns the first-order derivative of the first log-likelihood gradient
   *  for some trial value of the first mean u1 at a given value of x.
   *
   *  @param xk the given value of x
   *  @param u the trial values for u
   */
  def j11 = (xk: Double, u: DenseMatrix[Double]) => (pow(xk - u(0, 0), 2) *
    exp(pow(xk - u(0, 0), 2) / -2.0) * exp(pow(xk - u(1, 0), 2) / -2.0) -
    pow(exp(pow(xk - u(0, 0), 2) / -2.0), 2) -
    exp(pow(xk - u(0, 0), 2) / -2.0) * exp(pow(xk - u(1, 0), 2) / -2.0)) /
    pow(exp(pow(xk - u(0, 0), 2) / -2.0) + exp(pow(xk - u(1, 0), 2) / -2.0), 2)

  /** Returns the first-order derivative of the first log-likelihood gradient
   *  for some trial value of the second mean u1 at a given value of x, which
   *  also equals the derivative of the second gradient for mean u2.
   *
   *  @param xk the given value of x
   *  @param u the trial values for u
   */
  def j12 = (xk: Double, u: DenseMatrix[Double]) => -1 * (xk - u(0, 0)) *
    exp(pow(xk - u(0, 0), 2) / -2.0) * (xk - u(1, 0)) *
    exp(pow(xk - u(1, 0), 2) / -2.0) / pow(exp(pow(xk - u(0, 0), 2) / -2.0) +
    exp(pow(xk - u(1, 0), 2) / -2.0), 2)

  /** Returns the first-order derivative of the second log-likelihood gradient
   *  for some trial value of the second mean u2 at a given value of x.
   *
   *  @param xk the given value of x
   *  @param u the trial values for u
   */
  def j22 = (xk: Double, u: DenseMatrix[Double]) => (pow(xk - u(1, 0), 2) *
    exp(pow(xk - u(0, 0), 2) / -2.0) * exp(pow(xk - u(1, 0), 2) / -2.0) -
    pow(exp(pow(xk - u(1, 0), 2) / -2.0), 2) -
    exp(pow(xk - u(0, 0), 2) / -2.0) * exp(pow(xk - u(1, 0), 2) / -2.0)) /
    pow(exp(pow(xk - u(0, 0), 2) / -2.0) + exp(pow(xk - u(1, 0), 2) / -2.0), 2)

  /** Returns the Jacobian matrix of first-order derivatives for the log-
   *  likelihood function¡s gradient for some trial values of means u at all
   *  values of x in the sample.
   *
   *  @param u the trial values for u
   */
  def jacobian(u: DenseMatrix[Double]) = DenseMatrix((sum(x.map(j11(_, u))),
    sum(x.map(j12(_, u)))),(sum(x.map(j12(_, u))), sum(x.map(j22(_, u)))))

  // Running the approximations
  val values1 =
    algebra.newtonRaphson(function, jacobian, seedValues1, maxIterations)
  val values2 =
    algebra.newtonRaphson(function, jacobian, seedValues2, maxIterations)

  results.add(s"Saddle point: ${values1.t.map(utils.rounded(_, 6))}\n")
  results.add(s"Minimum: ${values2.t.map(utils.rounded(_, 6))}\n")

  // Writing the buffered results
  results.all('-', 80)
}
