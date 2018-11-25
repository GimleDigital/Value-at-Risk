package valueatrisk.chapter04

import java.io.File

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 4.2 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise04_02 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("4.2")

  // Exercise definitions
  val sample = csvread(new File("data/exercise-04-02-sample.dat"), '	')

  val h1 = max(sample)
  val h2 = (2.0 / sample.rows) * sum(sample)

  results.add(s"H1: ${h1}, H2: ${utils.rounded(h2, 4)}\n")

  // Writing the buffered results
  results.all('-', 80)
}
