package valueatrisk.chapter02

import scala.math.{exp, pow}

/** Proposed solution for exercise 2.19 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_19 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.19")

  /** Returns the function's value for a given value of x.
   *
   *  @param xk the value of x
   */
  def function = (xk: Double) => { exp(xk) - 3 * pow(xk, 2) }

  /** Returns the function's first order differential  for a given value of x.
   *
   *  @param xk the value of x
   */
  def differential = (xk: Double) => { exp(xk) - 6 * xk }

  /** Returns the value of x for which the function value is zero, by using
   *  Newton's method of approximation.
   *
   *  @param f the function that is evaluated
   *  @param d the function for calculating its first order differential
   *  @param xk the seed value of x, used for the first iteration
   */
  def newton(f: Double => Double, d: Double => Double, xk: Double): Double = {
    val x = xk - f(xk)/d(xk)
    val y = f(x) + d(x) * (x - xk)

    if (utils.rounded(y, 5) == 0) xk // Resolved
    else newton(f, d, x) // Not resolved, run next iteration
  }

  // Running the algorithm
  val seedValue = 0
  val approximation = newton(function, differential, 0)

  results.add(s"Approximation: x = ${utils.rounded(approximation, 5)}\n")

  // Writing the buffered results
  results.all('-', 80)
}