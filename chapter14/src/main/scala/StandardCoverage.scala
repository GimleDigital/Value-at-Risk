package valueatrisk.chapter14

import scala.math.max

case class StandardCoverage(alpha: Int, q: Double, epsilon: Double) {
  // Binomial helper functions
  val bin = new Binomial(alpha + 1, 1.0 - q)

  /** Returns the pair of values that lies outside the interval. */
  def getX = Array(bin.inversePmf(0, epsilon / 2.0), bin.inversePmf(0, 1.0 -
    epsilon / 2.0) + 1)

  /** Returns the four possible trial values of the probability that the number
   *  of exceedences lies outside the interval.*/
  def getTrials: Array[Double] = {
    val x = getX

    def getTrial(x0: Int, x1: Int) = {
      val lower = if (x0 < 0) 0.0 else bin.pmf(x0 - 1)
      val upper = 1 - bin.pmf(x1)

      lower + upper
    }

    val trials = new Array[Double](4)

    trials(0) = getTrial(x(0) + 1, x(1))
    trials(1) = getTrial(x(0) + 2, x(1))
    trials(2) = getTrial(x(0) + 1 , x(1) - 1)
    trials(3) = getTrial(x(0) + 2, x(1) - 1)

    trials
  }
}
