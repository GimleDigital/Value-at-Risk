package valueatrisk.chapter14

import utils._

/** Proposed solution for exercise 14.6 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise14_06 extends App {
  // Results are buffered and printed later
  val results = new Results("14.6")

  // Exercise definitions
  val a00 = 237.0
  val a01 = 5.0
  val a10 = 5.0
  val a11 = 2.0

  val confidenceLevel = 0.01

  // Christoffersen's Exceedence Independence Test
  val christoffersen = new Christoffersen(a00, a01, a10, a11)

  val likelihoodRatio = christoffersen.likelihoodRatio

  val targetLevel = christoffersen.target(1.0 - confidenceLevel)

  results.add(s"-2Log(Likelihood): ${likelihoodRatio}")
  results.add(s"Chi-square quantile: ${targetLevel}")

  val str = if (likelihoodRatio >= targetLevel) "" else "not "

  results.add(s"The Value-at-Risk measure is ${str}rejected")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
