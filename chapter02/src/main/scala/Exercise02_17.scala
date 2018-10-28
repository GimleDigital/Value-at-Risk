package valueatrisk.chapter02

import scala.math.pow
import scala.collection.mutable.ListBuffer

import java.io.File

import breeze.linalg._

/** Proposed solution for exercise 2.17 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_17 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.17")

  val x = DenseVector(0.0, 2.0, 4.0)
  val y = DenseVector(1.0, 2.0, 0.0)

  val size = x.size

  /** Expanding to obtain the f matrix will be done in three steps:
   *  1. For each condition, a row for p1 and another row for p2 is buffered
   *  2. Pairwise horizontal concatenation of the buffered rows
   *  3. Reducing the joint rows by vertical concatenation to form matrix f
   */
  val buffer = new ListBuffer[DenseMatrix[Double]]

  // Condition p1(x(0)) = y(0)
  buffer += DenseMatrix(pow(x(0), 3), pow(x(0), 2), pow(x(0), 1), pow(x(0), 0))
  buffer += DenseMatrix.zeros[Double](size + 1, 1)
  // Condition p1(x(1)) = y(1)
  buffer += DenseMatrix(pow(x(1), 3), pow(x(1), 2), pow(x(1), 1), pow(x(1), 0))
  buffer += DenseMatrix.zeros[Double](size + 1, 1)
  // Condition p2(x(1)) = y(1)
  buffer += DenseMatrix.zeros[Double](size + 1, 1)
  buffer += DenseMatrix(pow(x(1), 3), pow(x(1), 2), pow(x(1), 1), pow(x(1), 0))
  // Condition p2(x(2)) = y(2)
  buffer += DenseMatrix.zeros[Double](size + 1, 1)
  buffer += DenseMatrix(pow(x(2), 3), pow(x(2), 2), pow(x(2), 1), pow(x(2), 0))
  // Condition p1'(x1) - p2'(x1) = 0
  buffer += DenseMatrix(3 * pow(x(1), 2), 2 * x(1), 1, 0)
  buffer += DenseMatrix(-3 * pow(x(1), 2), -2 * x(1), -1, 0)
  // Condition p1''(x1) - p2''(x1) = 0
  buffer += DenseMatrix(6 * x(1), 2, 0, 0)
  buffer += DenseMatrix(-6 * x(1), -2, 0, 0)
  // Condition p1''(x0) = 0
  buffer += DenseMatrix(6 * x(0), 2, 0, 0)
  buffer += DenseMatrix.zeros[Double](size + 1, 1)
  // Condition p1''(x2) = 0
  buffer += DenseMatrix.zeros[Double](size + 1, 1)
  buffer += DenseMatrix(6 * x(2), 2, 0, 0)

  // Pairwise horizontal concatenation of the buffered rows
  val rows = new ListBuffer[DenseMatrix[Double]]

  for (i <- 0 to buffer.length / 2 - 1) {
    // the items are actually columns and have to be transposed
    rows += (DenseMatrix.horzcat(buffer(2*i).t, buffer(2*i+1).t))
  }

  // Reducing the joint rows by vertical concatenation to form matrix f
  val f = rows.reduceLeft(DenseMatrix.vertcat(_, _))

  results.add(s"Expanded f matrix:\n$f\n")

  /** The expanded y vector is obtained by repeating the second element and
   *  filling with zeros at the end. */
  val e1 = DenseMatrix(y(0), y(1), y(1), y(2))
  val e2 = DenseMatrix.zeros[Double](size + 1, 1)

  val ey = DenseMatrix.vertcat(e1, e2)
  results.add(s"Expanded y vector:\n$ey\n")

  /** Resolving for the B vector. */
  val b = inv(f) * ey

  results.add(s"B vector:")
  for (i <- 0 to b.rows - 1)
    results.add(s"${utils.rounded(b(i, 0), 3)}")
  results.add("")

  val bVector = b(::, 0)

  val polynomial1 = utils.getPolynomial((bVector(0 to 3)).toArray, "p1(x)", "x", 3)
  results.add(s"Polynomial 1: $polynomial1")
  val polynomial2 = utils.getPolynomial((bVector(4 to 7)).toArray, "p2(x)", "x", 3)
  results.add(s"Polynomial 2: $polynomial2\n")

  /** Writing the buffered results. */
  results.all('-', 80)
}