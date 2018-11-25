package valueatrisk.chapter03

import scala.math.{pow, sqrt}

import breeze.linalg.DenseVector

/** Proposed solution for exercise 3.12 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_12 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.12")

  // Exercise definitions
  val x1 = DenseVector(-1.0, 0.0, 1.0)

  val x2 = DenseVector(1.0, 0.0, 1.0)

  val p = DenseVector(0.25, 0.5, 0.25)

  // Example function
  results.add("Function x2 = pow(x1, 2) for x1 in (-1, 0, 1)\n")

   // 1) Obtaining the means
  val mean1 = (x1.t * p)
  val mean2 = (x2.t * p)

  val u1 = utils.rounded(mean1, 3)
  val u2 = utils.rounded(mean2, 3)

  results.add(s"1) Means: $u1, $u2\n")

  // 2) Obtaining the standard deviations
  val stdDev1 = sqrt((x1 - mean1).map(pow(_, 2)).t * p)
  val stdDev2 = sqrt((x2 - mean2).map(pow(_, 2)).t * p)

  val s1 = utils.rounded(stdDev1, 3)
  val s2 = utils.rounded(stdDev2, 3)

  results.add(s"2) Standard deviations: $s1, $s2\n")

  // 3) Obtaining the covariance
  val n = x1.size

  val means1 = DenseVector.fill(n){mean1}
  val means2 = DenseVector.fill(n){mean2}

  val deviations1 = x1 - means1
  val deviations2 = x2 - means1

  val covariance = (x1 - means1 :* x2 - means2 :* p).toArray.reduce(_ + _)

  results.add(s"3) Covariance: ${utils.rounded(covariance, 3)}\n")

  // Writing the buffered results
  results.all('-', 80)
}
