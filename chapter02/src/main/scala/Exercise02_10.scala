package valueatrisk.chapter02

import java.io._

import breeze.linalg._

/** Proposed solution for exercise 2.10 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_10 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.10")

  val A = csvread(new File("data/exercise-02-10-a.dat"),'	')
  val B = csvread(new File("data/exercise-02-10-b.dat"),'	')
  val C = csvread(new File("data/exercise-02-10-c.dat"),'	')

  results.add(utils.getCholesky(A, "a) "))
  results.add(utils.getCholesky(B, "b) "))
  results.add(utils.getCholesky(C, "c) "))

  // Writing the buffered results
  results.all('-', 80)
}