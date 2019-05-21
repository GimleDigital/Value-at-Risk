package valueatrisk.chapter08

import utils._

import scala.collection.mutable.ArrayBuffer

import java.io._

import breeze.linalg._

/** Proposed solution for exercise 8.1 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 *  This program is also applicable to exercises 8.2 - 8.4 by
 *  changing the variables startDate and openDays below
 */
object Exercise08_01 extends App {
  // Results are buffered and printed later
  val results = new Results("8.1")

  // Exercise definition
  val calender = csvread(new File("data/exercise-08-01-sample.dat"), '	')
    .map(_.toInt)

  val startDate = 20021114
  val openDays = 10

  /** Returns the number of basis days by counting the number of actual days
   *  after the start date until the specified number of open days has been
   *  reached.
   *
   *  @param i the current position in the calender
   *  @param dayCount the current number of open days
   *  @param actualDays the current number of actual days
   */
  def daysCounter(i: Int, dayCount: Int, actualDays: Int): Int = {
    if (calender(i, 0) < startDate) daysCounter(i + 1, dayCount, actualDays)
    else if (dayCount == openDays) actualDays
    else daysCounter(i + 1, dayCount + calender(i + 1, 1), actualDays + 1)
  }

  val T = daysCounter(0, 0, 0)

  results.add(s"Number of basis days: $T")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
