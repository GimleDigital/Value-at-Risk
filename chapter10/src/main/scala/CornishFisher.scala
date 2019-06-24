package valueatrisk.chapter10

import scala.collection.mutable.ArrayBuffer
import scala.math.{pow, sqrt}

import breeze.numerics.erfinv

/** Provides the components of a Cornish.Fisher expansion for a distribution.
 *
 *  @param quantile the quantile of the distribution to be evaluated
 *  @param mean the distribution's mean
 *  @param stdDev the distribution's standard deviation
 *  @param g the g's involved when calculating the distribution's moments
 */

case class CornishFisher(quantile: Double, mean: Double, stdDev: Double,
                         g: Array[Double]) {
  // Normalized cumulants
  val cumulants = Array(g(0), g(1), g(2)/pow(stdDev, 3), g(3)/pow(stdDev, 4),
                        g(4)/pow(stdDev, 5))

  /** Returns a component of the Cornish-Fisher expansion.
   *
   *  @param k the index of the component
   */
  def getComponent(k: Int) = {
    val invF = sqrt(2.0) * erfinv(2 * quantile - 1)
    k match {
      case 1 => invF
      case 2 => (pow(invF, 2) - 1.0) * cumulants(2) / 6.0
      case 3 => (pow(invF, 3) - 3 * invF) * cumulants(3) / 24.0
      case 4 => -1.0 * (2.0 * pow(invF, 3) - 5.0 * invF) *
        pow(cumulants(2), 2) / 36.0
      case 5 => (pow(invF, 4) - 6.0 * pow(invF, 2) + 3.0) * cumulants(4) / 120.0
      case 6 => -1.0 * (pow(invF, 4) - 5.0 * pow(invF, 2) + 2.0) *
        cumulants(2) * cumulants(3) / 24.0
      case 7 => (12.0 * pow(invF, 4) - 53.0 * pow(invF, 2) + 17.0) *
        pow(cumulants(2), 3) /324.0
      case default => 0.0
    }
  }
}