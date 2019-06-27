package valueatrisk.chapter14

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, sqrt}

import java.io._

import breeze.linalg._
import breeze.numerics.erfinv
import breeze.stats.distributions.Gaussian

/** Proposed solution for exercise 14.11 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise14_11 extends App {
  // Results are buffered and printed later
  val results = new Results("14.11")

  // Exercise definitions
  val sample = csvread(new File("data/exercise-14-11-sample.dat"), ';')

  val gaussian = new Gaussian(0, 1)

  /** Returns the quantile of a normal distribution for a given probability.
   *
   *  @param mu the distribution's mean
   *  @param sigma the distribution's standard deviation
   *  @param p the probability
   */
  def inverseCdf(mu: Double, sigma: Double, p: Double): Double = {
    mu + sigma * sqrt(2) * erfinv(2 * p - 1)
  }

  // Transforming VaR to tN
  val factor99 = 2.236 //sqrt(2) * erfinv(2 * 0.99 - 1) // Approximately 2.326

  val conditionalStdDev = sample(::, 0).map(_ / factor99)

  val pl = sample(::, 1)

  val tU = (pl.map(-1.0 * _) :/ conditionalStdDev).map(gaussian.cdf(_))

  val tN = tU.map(inverseCdf(0.0, 1.0, _))

  // a) Sample autocorrelations
  val autoCorrelation = new AutoCorrelation(tN)

  val r = new ArrayBuffer[Double]

  for (k <- 1 to 5) r.append(autoCorrelation.coefficient(k))

  results.add("a) Sample autocorrelations")
  results.add("k  Coefficient")
  results.add("-  -----------")

  for (k <- 1 to 5) results.add(s"${k}  ${doubleToString(r(k - 1), 4)}")

  // b) Standard Independence Test
  val threshold = 0.274 // 125 observations, significance level 0.5

  val maxCorrelation = r.map(abs(_)).max

  val str = if (maxCorrelation > threshold) "" else "not "

  results.add("b) Standard Independence Test")
  results.add(s"Max. correlation: ${doubleToString(maxCorrelation, 4)}")
  results.add(s"Threshold: ${threshold}")
  results.add(s"The Value-at-Risk measure is ${str}rejected")

  // Printing the results
  results.all('-', 80)

  Thread.sleep(1000)
}
