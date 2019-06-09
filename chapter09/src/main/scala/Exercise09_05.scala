package valueatrisk.chapter09

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.{exp}

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 9.5 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise09_05 extends App {
  // Results are buffered and printed later
  val results = new Results("9.5")

  val black = new Black

  // Exercise definitions
  val strike = 1050.0
  val expiry = 28.0 / 365.0

  val u = DenseMatrix(1077.0, 0.2721, 0.0263)

  val S = DenseMatrix((2414.0, 0.66549176402843, -0.00992265015737697),
    (0.66549176402843, 0.00469107444536863, 2.18254942830864E-6),
    (-0.00992265015737697, 2.18254942830864E-6, 7.65012825686574E-7))

  def P(R1: Int, R2: Int, R3: Int) =
    390.0 * black.getCall(R1, strike, R2, R3, expiry).getOrElse("price", 0.0)

  // a) Selecting five points on the unit sphere
  val points = DenseMatrix((1.0, 0.0, 0.0),(0.0, 1.0, 0.0), (-1.0, 0.0, 0.0),
                           (0.0, -1.0, 0.0), (0.0, 0.0, -1.0))

  results.add("a) Points on the unit sphere:")
  results.add(s"$points")

  // b) Yielding realizations
  val q = DenseMatrix(2.0)

  val s1 = sqrt(S(0, 0))
  val s2 = sqrt(S(1, 1))
  val s3 = sqrt(S(2, 2))

  val s = diag(DenseVector(s1, s2, s3))

  val sm = diag(DenseVector(1.0 / s1, 1.0 / s2, 1.0 / s3))

  val p = sm * S * sm

  def r(i: Int): DenseMatrix[Double]  = {
    val pk = points(i, 0 to 2).t

    s * pk * (q / sqrt(pk.t * inv(p) * pk)) + u
  }

  val R = Array(r(0), r(1), r(2), r(3), r(4), u)

  results.add("b) Yielding realizations:")
  for (r <- R) results.add(s"${r.t.map(doubleToString(_, 4))}")

  // c) Applying Black's formula
  val B = DenseMatrix.zeros[Double](R.length, 1)

  for (i <- 0 to R.length - 1) {
    val r = R(i)
    B(i, 0) = 390 * black.getCall(r(0, 0), strike, r(2, 0), expiry, r(1, 0))
      .getOrElse("price", 0.0)
  }

  results.add("c) Applying Black's formula:")

  results.add(s"${B.map(doubleToString(_, 0))}")

  // d) Interpolation
  val M = DenseMatrix.zeros[Double](R.length, 6)

  for (i <- 0 to R.length - 1) {
    M(i, 0) = pow(R(i)(0, 0), 2)
    M(i, 1) = pow(R(i)(1, 0), 2)
    M(i, 2) = R(i)(0, 0)
    M(i, 3) = R(i)(1, 0)
    M(i, 4) = R(i)(2, 0)
    M(i, 5) = 1.0
  }

  val coef = inv(M) * B

  val c11 = doubleToString(coef(0, 0), 4)
  val c22 = s"${getSign(coef(1, 0))} ${doubleToString(abs(coef(1, 0)), 0)}"
  val b1 = s"${getSign(coef(2, 0))} ${doubleToString(abs(coef(2, 0)), 0)}"
  val b2 = s"${getSign(coef(3, 0))} ${doubleToString(abs(coef(3, 0)), 0)}"
  val b3 = s"${getSign(coef(4, 0))} ${doubleToString(abs(coef(4, 0)), 0)}"
  val a = s"${getSign(coef(5, 0))} ${doubleToString(abs(coef(5, 0)), 0)}"

  results.add("d) Interpolation:")
  results.add(
    s"${c11} * R1^2 ${c22} * R2^2 ${b1} * R1 ${b2} * R2 ${b3} * R3 $a")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
