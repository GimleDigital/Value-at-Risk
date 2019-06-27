package valueatrisk.chapter14

import scala.collection.mutable.ArrayBuffer
import scala.math.{pow, sqrt}

import breeze.linalg._

/** Provides a calculus of autocorrelation.
 *
 *  @param values the variable values
 */
case class AutoCorrelation(values: DenseVector[Double]) {
  val alpha = values.length - 1

  /** Returns the autocorrelation for a given lag.
   *
   *  @param k the lag
   */
  def coefficient(k: Int) = {
    val series1 = values(k to alpha)
    val series2 = values(0 to alpha - k)

    val means1 = DenseVector.fill(alpha + 1 - k){series1.sum / series1.length}
    val means2 = DenseVector.fill(alpha + 1 - k){series2.sum / series2.length}

    val stdDev1 = sqrt((series1 - means1).map(pow(_, 2)).sum / series1.length)
    val stdDev2 = sqrt((series2 - means2).map(pow(_, 2)).sum / series2.length)

    val covariances = ((series1 - means1) :* (series2 - means2)).sum /
      (alpha + 1 - k)

    covariances / (stdDev1 * stdDev2)
  }
}