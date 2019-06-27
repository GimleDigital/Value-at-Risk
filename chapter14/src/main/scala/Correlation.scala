package valueatrisk.chapter14

import breeze.linalg._
import breeze.numerics._


/** Provides formulas for calculating the correlation between two variables.
 *
 *  @param values1 the values of the first variable
 *  @param values2 the values of the second variable
 */
case class Correlation (values1: DenseVector[Double],
  values2: DenseVector[Double]) {
  val length = values1.length

  val means1 = DenseVector.fill(length){values1.sum / length}
  val means2 = DenseVector.fill(length){values2.sum / length}

  val stdDev1 = sqrt((values1 - means1).map(pow(_, 2)).sum / (length - 1))
  val stdDev2 = sqrt((values2 - means2).map(pow(_, 2)).sum / (length - 1))

  val covariance = ((values1 - means1) :* (values2 - means2)).sum / (length - 1)

  val coefficient = covariance / (stdDev1 * stdDev2)
}
