package valueatrisk.chapter02

import java.io.File

import breeze.linalg._

/** Proposed solution for exercise 2.14 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_14 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.14")

  val x = csvread(new File("data/exercise-02-14-x.dat"),'	')
  val y = csvread(new File("data/exercise-02-14-y.dat"),'	')

  // Adding a column to x and filling with "1", to obtain f
  val ones = DenseMatrix.ones[Double](x.rows, x.cols)
  val f = DenseMatrix.horzcat(ones, x)

  results.add(s"f matrix:\n${f.toString}\n")
  results.add(s"y vector:\n${y.toString}\n")

  // Obtaining the unique minimum
  val b = (inv(f.t * f) * f.t * y).toDenseVector
  val b1 = utils.rounded(b(0), 1)
  val b2 = utils.rounded(b(1), 1)

  results.add(s"B vector:\n$b1\n$b2\n")

  val polynomial = utils.getPolynomial(b.toArray.reverse, "f(x)", "x", 2)
  results.add(s"$polynomial\n")

  // Writing the buffered results
  results.all('-', 80)
}
