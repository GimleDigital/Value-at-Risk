package valueatrisk.chapter14

import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt

// import breeze.linalg._
import breeze.numerics._

/** Provides some calculus related to binomial distributions.
 *
 *  @param n the number of trials
 *  @param p the probability of success
 */
case class Binomial (n: Int, p: Double) {
  /** Returns the factorial of a positive integer
   *
   *  @param k the integer
   */
  def factorial(k: BigInt): BigInt = {
    if (k == 0) 1
    else k * factorial(k - 1)
  }

  /** Returns the binomial coefficient for n and another positive BigIntegers.
   *
   *  @param k the lower BigInteger
   */
  def binom(k: BigInt) = factorial(n) / (factorial(k) * factorial(n - k))

  /** Returns the value of the probability mass function for a non-negative
   *  integer.
   *
   *  @param x the non-negative integer
   */
  def pmf(x: Int) = {
    val probability = new ArrayBuffer[Double]

    for (i <- 0 to x) probability.append(binom(i).toDouble * pow(p, i) *
      pow(1 - p, n - i))

    probability.sum
  }

  /** Returns the maximum integer value for which the inverse probability
   *  mass function is less than a given confidence level. Note that if zero (0)
   *  doesn't fulfil this condition, -1 is returned.
   *
   *  @param level the confidence level
   */
  def inversePmf(k: Int, level: Double): Int = {
    if (pmf(k) > level) k - 1
    else inversePmf(k + 1, level)
  }
}