package valueatrisk.chapter02

import java.io._

import scala.math

import breeze.linalg._

/** Proposed solutions for exercise 2.4 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise02_04 extends App {
  // Uploading tab separated data from files.
  val y = csvread(new File("data/exercise-02-04-y.dat"),'	')
  val x = csvread(new File("data/exercise-02-04-x.dat"),'	')

  // Resolving for the B vector
  val solution = (inv(x) * y).toDenseVector

  // Preparing the iteration for adding results to buffer
  val length = solution.length
  val range = (1 to length)

  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.4")

  // Individual B parameters
  for (i <- range) {
    val b =
      s"B${i} = ${utils.rounded(solution(length - i).toDouble, 2)}\n"
    results.add(b)
  }

  // The full equation
  val polynomial =
    utils.getPolynomial(solution.toArray.map(_.toDouble), "f(x)", "x", 2)
  results.add(s"$polynomial\n")

  // Writing the buffered results
  results.all('-', 80)
}
