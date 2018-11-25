package valueatrisk.chapter02

import java.io._

import breeze.linalg._

/** Proposed solution for exercise 2.13 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise02_13 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.13")

  val c = csvread(new File("data/exercise-02-13-c.dat"),'	')
  val b = csvread(new File("data/exercise-02-13-b.dat"),'	')

  // a) Matrix form of the polynomial
  results.add(s"a) Matrix form of the polynomial:\n")
  results.add(s"c:\n$c\n")
  results.add(s"b:\n$b\n")
  results.add(s"a = 5\n")

  // b) Cholesky matrix
  val ch = utils.getCholesky(c, "b) ")
  results.add(s"$ch\n")

  // c) Solving for the point x
  val solution = (-0.5 * inv(c) * b.t).toDenseVector

  val length = solution.length
  val range = (0 to length - 1)

  results.add(s"c) Solution:")
  for (i <- range) {
    results.add(s"x${i + 1} = ${solution(i)}\n")
  }

  // d) The polynomial’s value at the point x
  val value1 = solution.t * c * solution
  val value2 = (b * solution)
  results.add(s"d) Value = ${value1 + value2(0) + 5.0}\n")

  // Writing the buffered results
  results.all('-', 80)
}
