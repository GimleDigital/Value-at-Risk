package valueatrisk.chapter14

import utils._

import scala.collection.mutable.ArrayBuffer

import java.io._

import breeze.linalg._

/** Proposed solution for exercise 14.10 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise14_10 extends App {
  // Results are buffered and printed later
  val results = new Results("14.10")

  // Exercise definitions
  val sample = csvread(new File("data/exercise-14-10-sample.dat"), ';')

  /** Returns a count of consecutive observation pairs according to if there are
   *  exceedences or not.
   *
   *  @param i the number of iterations so far
   *  @param last the indicator if last record was an exceedence or not
   *  @param a the counters for each of the four possible sequences
   */
  def exceedences(i: Int, last: Int, a: DenseMatrix[Int]): DenseMatrix[Int] = {
    if (i > sample.rows - 1) a
    else {
      val next = if (sample(i, 0) < -1.0 * sample(i, 1)) 1 else 0

      {10 * last + next} match  {
        case 0 => a(0, 0) = a(0, 0) + 1 // sequence 0, 0
        case 1 => a(0, 1) = a(0, 1) + 1 // sequence 0, 1
        case 10 => a(1, 0) = a(1, 0) + 1 // sequence 1, 0
        case 11 => a(1, 1) = a(1, 1) + 1 // sequence 1, 1
      }

      exceedences(i + 1, next, a)
    }
  }

  // a) Alpha coefficients
  val a = exceedences(1, 0, DenseMatrix.zeros(2, 2))

  results.add("a) Alpha coefficients")
  results.add(
    s"a00: ${a(0, 0)}, a01: ${a(0, 1)}, a10: ${a(1, 0)}, a11: ${a(1, 1)}")

  // b) Values of q
  val c = new Christoffersen(a(0, 0), a(0, 1), a(1, 0), a(1, 1))

  val q0 = doubleToString(c.q0, 4)
  val q1 = doubleToString(c.q1, 4)
  val q = doubleToString(c.q, 4)

  results.add("b) Values of q")
  results.add(s"q0*: ${q0}, q1*: ${q1}, q*: ${q}")

  // Printing the results
  results.all('-', 80)

  Thread.sleep(1000)
}
