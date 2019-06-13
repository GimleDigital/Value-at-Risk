package valueatrisk.chapter03

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math._

/** Provides some statistical functions for a non-central chi-squared
 *  distribution, related to a quadratic function.
 *
 *  @param alpha the alpha parameter of the function
 *  @param beta the beta parameter of the function
 *  @param gamma the gamma parameters of the function
 *  @param variance the variances of the distribution
 */
case class NonCentralChiSquare(alpha: Double, beta: Double,
                               gamma: Array[Double], variance: Array[Double]) {
  // The number of parameters to handle
  val size = gamma.length

  /** Returns the partial expression A of the conditional CDF
   *
   *  @param w the quantile
   */
  def exprA(w: Double): Double = {
    val arr = new Array[Double](size)

    for (k <- 0 to size - 1) arr(k) = pow(gamma(k), 2) * variance(k) /
      (1.0 + 4.0 * pow(gamma(k) * w, 2))

    -0.5 * pow(w, 2) * (pow(beta, 2) + 4.0 * arr.sum)
  }

   /** Returns the partial expression B of the conditional CDF
   *
   *  @param w the quantile
   *  @param p the value of function p
   */
  def exprB(w: Double, p: Double): Double = {
    val arr = new Array[Double](size)

    for (k <- 0 to size - 1) arr(k) = gamma(k) * variance(k) /
      (1.0 + 4.0 * pow(gamma(k) * w, 2))

    w * (alpha - p + arr.sum)
  }

  /** Returns the partial expression C of the conditional CDF
   *
   *  @param w the quantile
   */
  def exprC(w: Double): Double = {
    val arr = new Array[Double](size)

    for (k <- 0 to size - 1) arr(k) = atan(2 * gamma(k) * w)

    0.5 * arr.sum
  }

  /** Returns the partial expression D of the conditional CDF
   *
   *  @param w the quantile
   */
  def exprD(w: Double): Double = {
    val arr = new Array[Double](size)

    for (i <- 0 to size - 1) arr(i) = 1.0 + 4.0 * pow(gamma(i) * w, 2)

    w * pow(arr.reduceLeft(_ * _), 0.25)
  }

  /** Returns the trapezoid approximation of the integral between two values of
   *  w at a given value of P.
   *
   *  @param w1 the lower value of w
   *  @param w2 the upper value of w
   *  @param p the value of P
   */
  def trapezoid(w1: Double, w2: Double, p: Double) = {
    var part1 = 0.0
    if (w1 == 0.0) {
      val arr = new Array[Double](size)
      for (k <- 0 to size - 1) arr(k) = gamma(k) * (variance(k) + 1.0)

      part1 = alpha - p + arr.sum
    }
    else {
      part1 = exp(exprA(w1)) * sin(exprB(w1, p) + exprC(w1)) / exprD(w1)
    }

    val part2 = exp(exprA(w2)) * sin(exprB(w2, p) + exprC(w2)) / exprD(w2)

    (part2 + part1) * (w2 - w1) / 2.0
  }

  /** Returns an approximation of the cumulative distribution function for a
   *  value of P.
   *
   *  @param wMax the upper limit for the evaluation of the integral part
   *  @param n the number of intervals for the trapezoid approximation
   *  @param p the value of P
   */
  def cdf(wMax: Double, n: Int, p: Double) = {
    val arr = new ArrayBuffer[Double]

    for (k <- 0 to n) arr.append(trapezoid(k * wMax / n, (k + 1) * wMax / n, p))

    0.5 - (1.0 / Pi) * arr.sum
  }

  /** Returns an estimation of the value of P that corresponds to a given
   *  quantile.
   *
   *  @param q the quantile
   *  @param wMax the upper limit for the evaluation of the integral part
   *  @param n the number of intervals for the trapezoid approximation
   *  @param p0 the lower initial value of P to be tested
   *  @param pk the upper initial value of P to be tested
   *  @param iter the current number of iterations
   *  @param maxIter the maximum number of iterations (a safety measure)
   */
  def pValue(q: Double, wMax: Double, n: Int, p0: Double, pk: Double, iter: Int,
             maxIter: Int = 100): Double = {
    if (iter > maxIter) {
      println(
        s"The limit of $maxIter iterations was reached, process stopped.")
      0.0
    }
    else {
      val p = pk - ((cdf(wMax, n, pk) - q) * (pk - p0)) / (cdf(wMax, n, pk) -
        cdf(wMax, n, p0))

      if (rounded(cdf(wMax, n, p), 6) == q) p
      else pValue(q, wMax, n, pk, p, iter + 1, maxIter)
    }
  }
}
