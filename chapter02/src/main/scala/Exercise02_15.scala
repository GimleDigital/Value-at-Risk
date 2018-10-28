package valueatrisk.chapter02

import scala.collection.mutable.ArrayBuffer
import scala.math.log

import java.io.File

import breeze.linalg._

/** Proposed solution for exercise 2.15 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_15 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.15")

  val x = csvread(new File("data/exercise-02-15-x.dat"),'	')
  val y = csvread(new File("data/exercise-02-15-y.dat"),'	')

  // Obtaining the f matrix
  val arrayBuffer = new ArrayBuffer[Double]
  for (i <- 0 to x.rows - 1) {
    arrayBuffer += utils.rounded(log(x(i, 0) * x(i, 1)), 3)
  }

  val z = new DenseVector(arrayBuffer.toArray).toDenseMatrix
  val ones = DenseMatrix.ones[Double](5, 1)
  val f = DenseMatrix.horzcat(ones, z.t)

  results.add(s"f matrix:\n${f.toString}\n")

  // Obtaining the unique minimum
  val b = (inv(f.t * f) * f.t * y).toDenseVector
  val b1 = utils.rounded(b(0), 2)
  val b2 = utils.rounded(b(1), 2)

  results.add(s"B vector:\n$b1\n$b2\n")

  val polynomial = utils.getPolynomial(b.toArray.reverse, "f(x)", "x", 2)
  results.add(s"$polynomial\n")

  // Writing the buffered results
  results.all('-', 80)
}