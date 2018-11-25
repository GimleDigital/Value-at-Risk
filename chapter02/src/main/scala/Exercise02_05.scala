package valueatrisk.chapter02

import scala.math.pow

import breeze.linalg._
import breeze.numerics.constants._

/** Proposed solutions for exercise 2.5 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise02_05 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.5")

  val f = DenseMatrix((utils.rounded(E, 4), utils.rounded(E, 4)),
                      (utils.rounded(pow(E,2), 4), utils.rounded(1.0, 4)))
  results.add(s"f matrix:\n${f.toString}\n")

  val y = DenseMatrix(1.0, 1.0)
  results.add(s"y vector:\n${y.toString}\n")

  val B = inv(f) * y
  val B1 = utils.rounded(B(0,0), 4)
  val B2 = utils.rounded(B(1,0), 4)
  results.add(s"B1 = $B1, B2 = $B2\n")

  results.add(s"f(x1,x2) = ${B1}exp(x1 + x2) + ${B2}exp(x1 - x2)\n")

  // Writing the buffered results
  results.all('-', 80)
}
