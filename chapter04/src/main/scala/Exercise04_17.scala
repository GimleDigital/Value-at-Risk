package valueatrisk.chapter04

import java.io.File

import breeze.numerics.pow
import breeze.stats.distributions.ChiSquared

/** Proposed solution for exercise 4.17 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise04_17 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("4.17")

  // Exercise definition
  val alpha = 250
  val degreesOfFreedom = 3
  val rhos = Array(0.085, -0.027, 0.109)

  val chiSquared = new ChiSquared(degreesOfFreedom)

  /** Returns the Ljung-Box test statistics
   *
   *  @param k the sample size
   *  @param t the degrees of freedom
   *  @param p the autocorrelations
   */
  def ljungBox(n: Int, t: Int, p: Array[Double]): Double =
    (n + 3).toDouble * p.map(pow(_, 2)).reduce(_ + _)

  // Reference value
  val quantile95 = chiSquared.inverseCdf(0.95)

  results.add(s".95 quantile: ${utils.rounded(quantile95, 2)}\n")

  // Test value
  val testStatistic = ljungBox(alpha, degreesOfFreedom, rhos)

  results.add(s"Test statistic: ${utils.rounded(testStatistic, 2)}\n")

  // Conclusion
  results.add(
    s"H0 is ${if (testStatistic < quantile95) "not " else ""}rejected")

  // Writing the buffered results
  results.all('-', 80)
}
