package valueatrisk.chapter05

import utils._

import scala.math.{exp, log, pow, sqrt}

import org.apache.commons.math3.distribution.NormalDistribution

/** Proposed solution for exercise 5.6 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise05_06 extends App {
  // Results are buffered and printed later
  val results = new Results("5.6")

  // Excercise definitions
  val u = 0.762415

  val normalDistribution = new NormalDistribution

  // Calculating the results
  val prv01 = normalDistribution.inverseCumulativeProbability(u)

  results.add(s"N(0,1) pseudorandom variate: ${rounded(prv01, 6)}")

  val m = log(pow(1.0, 2)/sqrt(pow(3.0, 2) + pow(1.0, 2)))

  results.add(s"m = ${rounded(m, 6)}")

  val s = sqrt(log(pow(3.0 / 1.0, 2)+ 1.0))

  results.add(s"s = ${rounded(s, 6)}")

  val prvms = s * prv01 + m

  results.add(s"N(m,s) pseudorandom variate: ${rounded(prvms, 6)}")

  val lnprv = exp(prvms)

  results.add(s"Lognormal pseudorandom variate: ${rounded(lnprv, 6)}")

  // Printing the results
  results.all('-', 80)
}
