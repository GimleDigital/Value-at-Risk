package valueatrisk.chapter14

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt

import java.io._

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 14.1 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise14_01 extends App {
  // Results are buffered and printed later
  val results = new Results("14.1")

  // Exercise definitions
  val alpha = 374
  val q = 0.90
  val epsilon = 0.05

  // 1) Initial interval
  val std = new StandardCoverage(alpha, q, epsilon)

  val x = std.getX

  results.add(s"1) Initial interval:")
  results.add(s"x1 = ${x(0) + 1}, x2 = ${x(1)}")

  // 2) Possible rejection intervals
  val trials = std.getTrials
  
  results.add(s"2) Possible rejection intervals:")
  results.add(s"x1  x2  Probability")
  results.add(s"--  --  -----------")
  results.add(s"${x(0) + 1}  ${x(1)}  ${doubleToString(trials(0), 3)}")
  results.add(s"${x(0) + 2}  ${x(1)}  ${doubleToString(trials(1), 3)}")
  results.add(s"${x(0) + 1}  ${x(1) - 1}  ${doubleToString(trials(2), 3)}")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
