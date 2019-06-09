package valueatrisk.chapter09

/** Represents a foreign currency option.
 *
 *  @param n the notional amount
 *  @param x the strike rate
 *  @param y the number of years to expiry
 *  @param r1 the compounded interest rate until expiry for currency 1
 *  @param r2 the compounded interest rate until expiry for currency 2
 *  @param v the implied volatility for the strike and maturity
 */
case class Option(n: Double, x: Double, y: Double, rate1: Double => Double, rate2: Double => Double,
             volatility: (Double, Double) => Double) {
  val r1 = rate1(y)
  val r2 = rate2(y)
  val v = volatility(x, y)
}
