package valueatrisk.chapter09

import scala.math.{exp, log, pow, sqrt}

import breeze.linalg._
import breeze.stats.distributions.Gaussian

/** Provides formulas for calculating value, delta and vega according to the
 *  Garman and Kohlhagen model for foreign currency options.
 *
 *  @param s the current exchange rate
 */
class Valuation(s: Double) {
  // Standard normal distribution
  private val g = new Gaussian(0, 1)

  /** Returns the option price.
   *
   *  @param o the option to be valued
   */
  def getPrice(o: Option) = {
    // Intermediate coefficients
    val d1 = (log(s / o.x) + (o.r1 - o.r2 + pow(o.v, 2) / 2) * o.y) /
             (o.v * sqrt(o.y))
    val d2 = d1 - o.v * sqrt(o.y)

    // Option price
    o.n * (s * exp(-1.0 * o.r2 * o.y) * g.cdf(d1) -
           o.x * exp(-1.0 * o.r1 * o.y) * g.cdf(d2))
  }

  /** Returns the option delta.
   *
   *  @param o the option to be valued
   */
  def getDelta(o: Option) = {
    // Intermediate coefficients
    val d1 = (log(s / o.x) + (o.r1 - o.r2 + pow(o.v, 2) / 2) * o.y) /
             (o.v * sqrt(o.y))

    // Option delta
    o.n * exp(-1.0 * o.r2 * o.y) * g.cdf(d1)
  }

  /** Returns the option vega.
   *
   *  @param o the option to be valued
   */
  def getVega(o: Option) = {
    // Intermediate coefficients
    val d1 = (log(s / o.x) + (o.r1 - o.r2 + pow(o.v, 2) / 2) * o.y) /
             (o.v * sqrt(o.y))

    // Option vega
    o.n * s * exp(-1.0 * o.r2 * o.y) * g.pdf(d1) * sqrt(o.y)
  }
}