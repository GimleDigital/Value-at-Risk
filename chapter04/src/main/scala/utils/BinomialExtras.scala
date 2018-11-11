package valueatrisk.chapter04

import breeze.stats.distributions.Binomial

/** Adds functionalities to the Breeze Binomial distribution class.
 *
 *  @param n the number of draws, i.e items in the sample
 *  @param p the distribution's probability
 */
case class BinomialExtras(n: Int, p: Double) {
  val binomial = new Binomial(n, p)

  /** Returns the value of the Cumulative Distribution Function.
   *
   *  @param k the upper limit of successful draws
   */
  def cdf(k: Int): Double = {
    /** Returns the accumulated value plus the increment for the kth iteration.
     *
     *  @param accumulated the previously accumulated value
     *  @param iteration the current number of draw
     */
    def increment(accumulated: Double, iteration: Int): Double = {
      iteration match {
        case `k` => accumulated + binomial.probabilityOf(iteration)
        case _ => increment(accumulated + binomial.probabilityOf(iteration),
                            iteration + 1)
      }
    }

    increment(0.0, 0)
  }
}
