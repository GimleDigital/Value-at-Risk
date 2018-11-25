package valueatrisk.chapter03

import scala.math.{pow, sqrt}

import breeze.linalg.DenseVector

/** Proposed solution for exercise 3.11 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_11 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.11")

  // Exercise definitions
  val q1 = DenseVector(1.0, 1.0, 2.0, 0.0, 3.0)

  val q2 = DenseVector(0.0, 3.0, 1.0, 3.0, 2.0)

  val p = DenseVector(0.3, 0.1, 0.2, 0.1, 0.3)

   // 1) Obtaining the means
  val mean1 = (q1.t * p)
  val mean2 = (q2.t * p)

  val u1 = utils.rounded(mean1, 3)
  val u2 = utils.rounded(mean2, 3)

  results.add(s"1) Means: $u1, $u2\n")

  // 2) Obtaining the standard deviations
  val stdDev1 = sqrt((q1 - mean1).map(pow(_, 2)).t * p)
  val stdDev2 = sqrt((q2 - mean2).map(pow(_, 2)).t * p)

  val s1 = utils.rounded(stdDev1, 3)
  val s2 = utils.rounded(stdDev2, 3)

  results.add(s"2) Standard deviations: $s1, $s2\n")

  // 3) Obtaining the covariance
  val n = q1.size

  val means1 = DenseVector.fill(n){mean1}
  val means2 = DenseVector.fill(n){mean2}

  val deviations1 = q1 - means1
  val deviations2 = q2 - means1

  val covariance = (q1 - means1 :* q2 - means2 :* p).toArray.reduce(_ + _)

  results.add(s"3) Covariance: ${utils.rounded(covariance, 3)}\n")

  // 4) Obtaining the correlation

  val correlation = covariance / (stdDev1 * stdDev2)

  results.add(s"4) Correlation: ${utils.rounded(correlation, 3)}\n")

  // Writing the buffered results
  results.all('-', 80)
}
