package valueatrisk.chapter02

import scala.math.{exp, pow}

/** Proposed solution for exercise 2.20 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_20 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.20")

  /** Returns the function's value for a given value of x.
   *
   *  @param xk the value of x
   */
  def function = (xk: Double) => { exp(xk) - 3 * pow(xk, 2) }

  /** Returns the value of x for which the function value is zero, by using
   *  the secant method of approximation.
   *
   *  @param f the function that is evaluated
   *  @param d the function for calculating its first order differential
   *  @param xk the seed value of x, used for the first iteration
   */
  def secant(f: Double => Double, x0: Double, xk: Double): Double = {
    val x = xk - (f(xk) * (xk - x0)) / (f(xk) - f(x0))
    val y = (f(xk) - f(x0)) / (xk - x0)

    if (utils.rounded(y, 5) == 0) xk // Resolved
    else secant(f, xk, x) // Not resolved, run next iteration
  }

  // Running the algorithm
  val seedValues = (0.0, 1.0)
  val approximation = secant(function, seedValues._1, seedValues._2)

  results.add(s"Approximation: x = ${utils.rounded(approximation, 5)}\n")

  // Writing the buffered results
  results.all('-', 80)
}