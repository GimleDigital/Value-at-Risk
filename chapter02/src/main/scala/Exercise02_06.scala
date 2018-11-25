package valueatrisk.chapter02

import scala.math.{cos, sin}

import breeze.math.Complex
import breeze.numerics.pow
import breeze.numerics.constants._

/** Proposed solution for exercise 2.6 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */object Exercise02_06 extends App {
  // Results will be stored in a buffer, to be written later.
  val results = new utils.Results("2.6")

  val a1 = Complex(5, 3)
  val a2 = Complex(2, -1)
  val resultA = a1*a2
  results.add(s"a) $resultA\n")

  val resultB = 5 / Complex(2, 1)
  results.add(s"b) $resultB\n")

  // Applying Euler's formula
  val resultC = Complex(utils.rounded(cos(1),4), utils.rounded(sin(1), 4))
  results.add(s"c) $resultC\n")

  // Writing the buffered results
  results.all('-', 80)
}