package valueatrisk.chapter02

import scala.math.{log, pow}
import scala.collection.mutable.ListBuffer

import breeze.linalg.{det, DenseMatrix}

/** Proposed solution for exercise 2.24 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise02_24 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.24")

  // Exercise definitions
  val xMin = 1.0
  val xMax = 2.0

  val iterations = 2000
  val steps = 10

  def y = (x: Double) => 1 / x
  def F = (x: Double) => log(x)

  /** Returns the analytical solution of an integral.
   *
   *  @param f the function to be evaluated
   *  @param min the start value for the integral
   *  @param max the end value for the integral
   */
  def analyticalIntegral(f: Double => Double, min: Double,
                         max: Double): Double = {
    f(max) - f(min)
  }

  /** Returns the approximation of an integral, obtained via Riemann sums.
   *
   *  @param f the function to be evaluated
   *  @param min the start value for the integral
   *  @param max the end value for the integral
   *  @param m the number of steps
   */

  def riemannIntegral(f: Double => Double, min: Double, max: Double,
                      m: Int): Double = {
    val buffer = new ListBuffer[Double]

    val step = (max - min) / m.toDouble

    for ( i <- 1 to m) {
      buffer += f(min + i.toDouble * step) * step
    }

    buffer.reduce(_ + _)
  }

  /** Returns the approximation of an integral, obtained via the trapezoidal
   *  rule.
   *
   *  @param f the function to be evaluated
   *  @param min the start value for the integral
   *  @param max the end value for the integral
   *  @param m the number of steps
   */
  def trapezoidalIntegral(f:Double => Double, min: Double, max: Double,
                      m: Int): Double = {
    val step = (max - min) / m.toDouble

    /** Returns the approximated value, after m iterations
     *
     *  @param iter the remaining iterations
     *  @param prev the accumulated value so far
     *  @param x0 the min value of x
     */
    def trapezoid(iter: Int, prev: Double, x0: Double): Double = {
      if (iter == 0) prev
      else {
        val x1 = x0 + step

        val increment = (f(x0) + f(x1)) * step / 2.0

        trapezoid(iter - 1, prev + increment, x1)
      }
    }

    trapezoid(m, 0.0, min)
  }

  /** Returns the approximation of an integral,
   *
   *  @param f the function to be evaluated
   *  @param min the start value for the integral
   *  @param max the end value for the integral
   *  @param m the number of iterations
   */
  def simpsonIntegral(f:Double => Double, min: Double, max: Double,
                      m: Int): Double = {
    val step = (max - min) / m.toDouble

    /** Returns the approximated value, after m iterations
     *
     *  @param iter the remaining iterations
     *  @param prev the accumulated value so far
     *  @param x0 the min value of x
     */
    def simpson(iter: Int, prev: Double, x0: Double): Double = {
      val increment = f(min + iter.toDouble * step) * step / 3
      val multiplier = if (iter % 2 == 1) 4.0 else 2.0

      iter match {
        case `m` => simpson(iter - 1, prev + increment, x0 + step)
        case 0 => prev + increment
        case _ => simpson(iter - 1, prev + multiplier * increment, x0 + step)
      }
    }

    simpson(m, 0, min)
  }

  // Obtaining the results
  val a = utils.rounded(analyticalIntegral(F, xMin, xMax), 5)
  val r = utils.rounded(riemannIntegral(y, xMin, xMax, steps), 5)
  val t = utils.rounded(trapezoidalIntegral(y, xMin, xMax, steps), 5)
  val s = utils.rounded(simpsonIntegral(y, xMin, xMax, steps), 5)

  // Writing results to buffer
  results.add(s"a) Analytical solution: ${a}\n")
  results.add(s"b) Riemann sums => ${r}\n")
  results.add(s"c) Trapezoidal rule => $t\n")
  results.add(s"d) Simpson's rule => $s\n")

  // Writing the buffered results
  results.all('-', 80)
}
