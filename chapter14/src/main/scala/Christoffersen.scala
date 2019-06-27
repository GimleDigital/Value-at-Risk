package valueatrisk.chapter14

import scala.math.{log, pow}

import breeze.stats.distributions.ChiSquared

/** Provides some calculus related to Christoffersen's Exceedence Independence
 *  Test.
 *
 *  @param a00 the number of cases where i(t-1) is 0 and i(t) is 0
 *  @param a01 the number of cases where i(t-1) is 0 and i(t) is 1
 *  @param a10 the number of cases where i(t-1) is 1 and i(t) is 0
 *  @param a11 the number of cases where i(t-1) is 1 and i(t) is 1
 */
case class Christoffersen(a00: Double, a01: Double, a10: Double, a11: Double) {
  // The "-2Log" approximations follows a chi-square distribution
  val chiSquared = new ChiSquared(1)

  // Estimations when H0 doesn't hold
  val q0 = a00 / (a00 + a01)
  val q1 = a10 / (a10 + a11)

  println(s"a00 = $a00, a01 = $a01, a00 + a01 = ${a00 + a01}")

  // Estimation when H0 does hold
  val q = (a00 + a10) / (a00 + a01 + a10 + a11)

  /** Returns the target value for the likelihood
   *
   *  @param significanceLevel the level of significance
   */
  def target(significanceLevel: Double) = chiSquared.inverseCdf(significanceLevel)

  /** Returns the likelihood for the alpha values */
  val likelihoodRatio = -2.0 * log(pow(q / q0, a00) * pow((1 - q) / (1 - q0),
    a01) * pow(q / q1, a10) * pow((1 - q) / (1 - q1), a11))
}