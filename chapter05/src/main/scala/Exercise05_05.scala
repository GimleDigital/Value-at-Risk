package valueatrisk.chapter05

import utils._

import org.apache.commons.math3.analysis.function.Sqrt
import org.apache.commons.math3.distribution.NormalDistribution

/** Proposed solution for exercise 5.5 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise05_05 extends App {
  // Results are buffered and printed later
  val results = new Results("5.5")

  // Excercise definitions
  val u = 0.983467

  val normalDistribution = new NormalDistribution

  // Calculating the results
  val prv01 = normalDistribution.inverseCumulativeProbability(u)

  results.add(s"N(0,1) pseudorandom variate: ${rounded(prv01, 6)}")

  val sqrt = new Sqrt

  val prv14 = sqrt.value(4.0) * prv01 + 1.0

  results.add(s"N(1,4) pseudorandom variate: ${rounded(prv14, 6)}")

  // Printing the results
  results.all('-', 80)
}
