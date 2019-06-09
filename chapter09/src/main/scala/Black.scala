package valueatrisk.chapter09

import scala.math.{exp, log, pow, sqrt}

import breeze.stats.distributions.Gaussian

/** Provides formulas for obtaining call and put prices and greeks for commodity
 *  options by using the Black (1976) method, as described at the website
 *  https://www.glynholton.com/notes/black_1976
 */
class Black {
  val g = new Gaussian(0, 1)

  /** Returns the price and greeks of a call option.
   *
   *  @param f the current underlying forward price
   *  @param x the strike price
   *  @param r the continuously compounded risk free interest rate
   *  @param t the time in years until the expiration of the option
   *  @param v the implied volatility for the underlying forward price
   */
  def getCall(f: Double, x: Double, r: Double, t: Double, v: Double) = {
    val d1 = (log(f / x) + t * pow(v, 2) / 2.0) / (v * sqrt(t))
    val d2 = d1 - v * sqrt(t)

    val e = exp(-1.0 * r * t)

    val price = e * (f * g.cdf(d1) - x * g.cdf(d2))
    val delta = e * g.cdf(d1)
    val gamma = e * g.pdf(d1) / (f * v * sqrt(t))
    val vega = f * e * g.pdf(d1) * sqrt(t)
    val theta = -1.0 * f * e * g.pdf(d1) * v / (2.0 * sqrt(t)) +
      r * f * e * g.cdf(d1) - r * x * e * g.cdf(d2)
    val rho = -1.0 * t * price

    Map("price" -> price, "delta" -> delta, "gamma" -> gamma, "vega" -> vega,
        "theta" -> theta, "rho" -> rho)
  }

  /** Returns the price and greeks of a put option.
   *
   *  @param f the current underlying forward price
   *  @param x the strike price
   *  @param r the continuously compounded risk free interest rate
   *  @param t the time in years until the expiration of the option
   *  @param v the implied volatility for the underlying forward price
   */
  def getPut(f: Double, x: Double, r: Double, t: Double, v: Double) = {
    val d1 = (log(f / x) + t * pow(v, 2) / 2.0) / (v * sqrt(t))
    val d2 = d1 - v * sqrt(t)

    println(s"$d1")

    val e = exp(-1.0 * r * t)

    val price = e * (f * g.cdf(-1.0 * d2) - x * g.cdf(-1.0 * d1))
    val delta = e * (g.cdf(d1) - 1.0)
    val gamma = e * g.pdf(d1) / (f * v * sqrt(t))
    val vega = f * e * g.pdf(d1) * sqrt(t)
    val theta = -1.0 * f * e * g.pdf(d1) * v / (2.0 * sqrt(t)) -
      r * f * e * g.cdf(-1.0 * d1) + r * x * e * g.cdf(-1.0 * d2)
    val rho = -1.0 * t * price

    Map("price" -> price, "delta" -> delta, "gamma" -> gamma, "vega" -> vega,
        "theta" -> theta, "rho" -> rho)
  }
}