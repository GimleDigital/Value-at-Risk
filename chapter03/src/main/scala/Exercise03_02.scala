package valueatrisk.chapter03

import scala.math.{pow, sqrt}

import breeze.linalg.DenseVector

/** Proposed solution for exercise 3.2 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise03_02 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.2")

  // Exercise definitions
  val observations = DenseVector(0.0, 1.0, 2.0, 3.0)
  val probabilities = DenseVector(0.125, 0.375, 0.375, 0.125)

  // 1. Obtaining the mean
  val mean = (observations.t * probabilities)
  results.add(s"a) Mean: $mean\n")

  // 2. Obtaining the variance
  val means = DenseVector.fill(4){mean}
  val variance = (observations - means).map(pow(_, 2)).t * probabilities
  results.add(s"b) Variance: $variance\n")

  // 3. Obtaining the standard deviation
  val stdDev = utils.rounded(sqrt(variance), 5)
  results.add(s"c) Standard deviation: $stdDev\n")

  // 4. Obtaining the skewness
  val skewness = utils.rounded((observations - means).map(pow(_, 3)).t *
                                probabilities, 5)
  results.add(s"d) Skewness: $skewness\n")

  // 5. Obtaining the kurtosis
  val kurtosis = utils.rounded((observations - means).map(pow(_, 4)).t *
                                probabilities, 5)
  results.add(s"e) Kurtosis: $kurtosis\n")

  // Writing the buffered results
  results.all('-', 80)
}
