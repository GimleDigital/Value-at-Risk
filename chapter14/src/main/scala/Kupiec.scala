package valueatrisk.chapter14

import scala.math.{log}

import breeze.stats.distributions.ChiSquared

/** Provides some calculus used in the Kupiec’s PF Coverage Test.
 *
 *  @param alpha the alpha coefficient
 *  @param epsilon the confidence interval
 */
case class Kupiec(alpha: Int, q: Double, epsilon: Double) {
  // The "-2Log" approximations follows a chi-square distribution
  val chiSquared = new ChiSquared(1)

  // The target value for the likelihood
  val target = chiSquared.inverseCdf(1 - epsilon)

  /** Returns the likelihood for a given value of x, using the "-2Log"
   *  approximation.
   *
   *  @param x the value of x to be tried
   */
  def likelihood(x: Int) = {
    2 * ((alpha + 1 - x) * log((alpha + 1 - x) / (q * (alpha + 1))) + x *
      log(x / ((1 - q) * (alpha + 1))))
  }

  /** Returns the two solutions for the "-2Log" approximations, where the
   *  likelihood equals the target value.
   *
   *  @param x the value to be tried
   *  @param solutions the array that shall be filled with the two solutions
   */
  def getSolutions(x: Int, solutions: Array[Int]): Array[Int] = {
    if (solutions.min > 0) solutions // Two solutions have been found
    else if (x > alpha) {
      println("ERROR: Maximum number of iterations have been reached")

      Array(0, 0)
    }
    else {
      val lh = likelihood(x)

      if (solutions(0) == 0) {
        if (lh < target) solutions(0) = x - 1
      }

      else {
        if (lh > target) solutions(1) = x
      }

      getSolutions(x + 1, solutions)
    }
  }
}