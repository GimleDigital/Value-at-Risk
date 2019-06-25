package valueatrisk.chapter11

import utils._

import java.io._

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.ChiSquared

/** Proposed solution for exercise 11.1 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise11_01 extends App {
  // Results are buffered and printed later
  val results = new Results("11.1")

  // Exercise definitions
  val r = csvread(new File("data/exercise-11-01-sample.dat"), ';')

  val confidenceLevel = 0.90

  val obs = r.rows // Number of observations

  // a) Direct estimation
  val rA = r.toArray

  scala.util.Sorting.quickSort(rA)

  val quantileA = (rA(obs - 1) + rA(obs - 2)) / 2.0

  results.add("a) Direct estimation")
  results.add(s"0.90 quantile: ${doubleToString(quantileA, 5)}")

  // b) Bell-shaped distribution
  val normal90 = 1.282

  val meanB = r.sum / obs

  val stdDevB = sqrt(r.map(_ - meanB).map(pow(_, 2)).sum / obs)

  val quantileB = meanB + pow(normal90, 2) * stdDevB

  results.add("b) Bell-shaped distribution")
  results.add(s"Mean: ${doubleToString(meanB, 5)}")
  results.add(s"Std. Dev: ${doubleToString(stdDevB, 5)}")
  results.add(s"0.90 quantile: ${doubleToString(quantileB, 5)}")

  // c) Actual quantile
  val chiSquared = new ChiSquared(12)

  val draws = DenseVector.zeros[Double](obs)

  for(i <- 0 to obs - 1) draws(i) = chiSquared.draw

  val rC = draws.toArray
  scala.util.Sorting.quickSort(rC)

  val quantileCA = (rC(obs - 1) + rC(obs - 2)) / 2.0

  val meanC = draws.sum / obs

  val stdDevC = sqrt(r.map(_ - meanC).map(pow(_, 2)).sum / obs)

  val quantileCB = meanC + normal90 * stdDevC

  val quantileCC = chiSquared.inverseCdf(confidenceLevel)

  results.add("c) Actual quantile")
  results.add(s"Direct estimation: ${doubleToString(quantileCA, 5)}")
  results.add(s"Bell shaped distribution: ${doubleToString(quantileCB, 5)}")
  results.add(s"Chi-square: ${doubleToString(quantileCC, 5)}")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
