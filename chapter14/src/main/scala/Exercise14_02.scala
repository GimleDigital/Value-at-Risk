package valueatrisk.chapter14

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt

import java.io._

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 14.2 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise14_02 extends App {
  // Results are buffered and printed later
  val results = new Results("14.2")

  // Exercise definitions
  val alpha = 374
  val q = 0.90
  val epsilon = 0.05

  // Obtaining the solutions
  val kupiec = new Kupiec(alpha, q, epsilon)

  val solutions = kupiec.getSolutions(1, Array(0, 0))

  results.add(s"Interval:")
  results.add(s"x1 = ${solutions(0)}, x2 = ${solutions(1)}")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
