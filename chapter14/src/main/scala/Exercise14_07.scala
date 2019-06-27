package valueatrisk.chapter14

import utils._

import scala.math.abs

/** Proposed solution for exercise 14.7 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise14_07 extends App {
  // Results are buffered and printed later
  val results = new Results("14.7")

  // Exercise definitions
  val significanceLevel = 0.05

  val threshold = 0.138 // 500 observations, significance level 0.5

  val autoCorrelations = Array(0.034, -0.078, -0.124, 0.107, 0.029)

  // Standard Independence Test
  val maxCorrelation = autoCorrelations.map(abs(_)).max

  val str = if (maxCorrelation > threshold) "" else "not "

  results.add(s"Max. correlation: ${maxCorrelation}")
  results.add(s"Threshold: ${threshold}")
  results.add(s"The Value-at-Risk measure is ${str}rejected")

  // Printing the results
  results.all('-', 80)

  Thread.sleep(1000)
}
