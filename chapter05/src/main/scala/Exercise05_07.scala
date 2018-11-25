package valueatrisk.chapter05

import utils._

import scala.math.{exp, log, pow, sqrt}

import org.apache.commons.math3.distribution.NormalDistribution

import breeze.linalg._

/** Proposed solution for exercise 5.7 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise05_07 extends App {
  // Results are buffered and printed later
  val results = new Results("5.7")

  // Excercise definitions
  val u1 = 0.194385
  val u2 = 0.843852
  val u3 = 0.665872

  val means = DenseMatrix(1.0, 0.0, 3.0)
  val sigmas = DenseMatrix((4.0, 0.0, 2.0), (0.0, 1.0, 1.0), (2.0, 1.0, 9.0))

  val normalDistribution = new NormalDistribution

  // 1. N3(0,I) Pseudorandom vector x
  val x = DenseMatrix(normalDistribution.inverseCumulativeProbability(u1),
                      normalDistribution.inverseCumulativeProbability(u2),
                      normalDistribution.inverseCumulativeProbability(u3))

  results.add(s"x1: ${rounded(x(0, 0), 6)}")
  results.add(s"x2: ${rounded(x(1, 0), 6)}")
  results.add(s"x3: ${rounded(x(2, 0), 6)}\n")

  // 2. Cholesky matrix
  val k = cholesky(sigmas)

  results.add(s"Cholesky matrix:\n${k.map(rounded(_, 6))}\n")

  // 3. N3(μ,Σ)Pseudorandom vector y
  val y = k * x + means

  results.add(s"y:\n${y.map(rounded(_, 6))}\n")

  // Printing the results
  results.all('-', 80)
}
