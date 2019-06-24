package valueatrisk.chapter10

import breeze.linalg._
import breeze.numerics._

/** Provides formulas to calculate the moments of a distribution.
 *
 *  @param u a matrix with the distribution's mean and some zero elements
 *  @param c a matrix with the estimated c coefficient and some zero elements
 *  @param b a matrix with the estimated b coefficient and some zero elements
 *  @param a a matrix with the estimated a coefficient
 *  @param cDot the remapped version of c
 *  @param bDot the remapped version of b
 *  @param aDot the remapped version of a
 */
case class Moments (u: DenseMatrix[Double], c: DenseMatrix[Double],
                    b: DenseMatrix[Double], a: DenseMatrix[Double],
                    cDot: DenseMatrix[Double], bDot: DenseMatrix[Double],
                    aDot: DenseMatrix[Double]) {
  /** Returns the factorial of a positive integer
   *
   *  @param k the integer
   */
  def factorial(k: Int): Int = {
    if (k == 0) 1
    else k * factorial(k - 1)
  }

  /** Returns the binomial coefficient for two positive integers.
   *
   *  @param n the upper integer
   *  @param k the lower integer
   */
  def binom(n: Int, k: Int) = {
    factorial(n) / (factorial(k) * factorial(n - k))
  }

  /** Returns one of the g components that are used when calculating the
   *  moments.
   *
   *  @param k the index of the g component
   */
  def getG(k: Int): Double = {
    k match {
      case 0 => {
        val m = u * c * u.t + b.t * u.t + a + DenseMatrix(sum(diag(cDot)))
        m(0, 0)
      }
      case k if (k > 0) => {
        val t1 = pow(bDot.t, 2) * pow(diag(cDot).map(_ * 2.0), k - 1)
        val t2 = sum(pow(diag(cDot).map(_ * 2.0), k + 1))

        0.5 * factorial(k + 1) * t1(0) + 0.5 * factorial(k) * t2
      }
      case default => 0.0
    }
  }

  /** Returns one of the distribution's moments.
   *
   *  @param k the index of the moment
   */
   def getEY(k: Int): Double = {
    k match {
      case 1 => getG(0)
      case 2 => getG(1) + binom(1, 1) * getG(0) * getEY(1)
      case 3 => getG(2) + binom(2, 1) * getG(1) * getEY(1) + binom(2, 2) *
        getG(0) * getEY(2)
      case 4 => getG(3) + binom(3, 1) * getG(2) * getEY(1) + binom(3, 2) *
        getG(1) * getEY(2) + binom(3, 3) * getG(0) * getEY(3)
      case 5 => getG(4) + binom(4, 1) * getG(3) * getEY(1) + binom(4, 2) *
        getG(2) * getEY(2) + binom(4, 3) * getG(1) * getEY(3) + binom(4, 4) *
        getG(0) * getEY(4)
      case default => 0.0
    }
  }

  /** Returns one of the distribution's centralized moments.
   *
   *  @param k the index of the moment
   */
  def getCM(k: Int): Double = {
    k match {
      case 1 => 0.0
      case 2 => getG(1) + binom(1, 1) * getG(0) * getCM(1)
      case 3 => getG(2) + binom(2, 1) * getG(1) * getCM(1) + binom(2, 2) *
        getG(0) * getCM(2)
      case 4 => getG(3) + binom(3, 1) * getG(2) * getCM(1) + binom(3, 2) *
        getG(1) * getCM(2) + binom(3, 3) * getG(0) * getCM(3)
      case 5 => getG(4) + binom(4, 1) * getG(3) * getCM(1) + binom(4, 2) *
        getG(2) * getCM(2) + binom(4, 3) * getG(1) * getCM(3) + binom(4, 4) *
        getG(0) * getCM(4)
      case default => 0.0
    }
  }
}