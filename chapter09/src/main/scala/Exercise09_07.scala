package valueatrisk.chapter09

import utils._

import scala.math.{abs}

import breeze.linalg._
import breeze.numerics.sqrt

/** Proposed solution for exercise 9.7 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise09_07 extends App {
  // Results are buffered and printed later
  val results = new Results("9.7")

  // Exercise definitions
  val r0 = DenseVector(18.75, 19.17, 19.29, 19.33, 19.37, 19.41, 19.43, 19.46,
    19.49, 19.51, 19.53, 19.55)
  val r = DenseVector(-1.07, -1.84, -1.55, -1.23)
  val k = DenseVector(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).map(_.toDouble)

  // 1. Interpolation function
  val k1 = DenseVector(k(0), k(4), k(10))
  val x = DenseMatrix(k1, sqrt(k1), DenseVector(1.0, 1.0, 1.0)).t
  val y = DenseMatrix(r(1 to 3)).t

  val z = inv(x) * y

  val c = doubleToString(z(0, 0), 4)
  val b = s"${getSign(z(1, 0))} ${doubleToString(abs(z(1, 0)), 4)}"
  val a = s"${getSign(z(2, 0))} ${doubleToString(abs(z(2, 0)), 4)}"

  results.add(s"1. Interpolation function:")
  results.add(s"$c * k $b * k^0.5 $a")

  // 2. Modelling changes
  val changes = DenseMatrix.horzcat(DenseMatrix(r(0)), DenseMatrix(z(0, 0) * k +
    z(1, 0) * sqrt(k) + z(2, 0)))

  results.add("2. Modelling changes in variable:")
  results.add(s"${changes.map(doubleToString(_, 2))}")

  // 3. Variable remapping
  val q = changes + r0.t

  results.add("3. Variable remapping:")
  results.add(s"${q.map(doubleToString(_, 2)
  )}")

  // 3.

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
