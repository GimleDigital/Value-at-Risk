package valueatrisk.chapter03

import scala.collection.mutable.ListBuffer

import java.io.File
import java.text.DecimalFormat
import java.util.Locale

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 3.20 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise03_20 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.20")

  val buffer = new ListBuffer[(String, DenseMatrix[Double])]

  // Exercise definitions is also buffered
  buffer.append(("A", csvread(new File("data/exercise-03-20-a.dat"),'	')))
  buffer.append(("B", csvread(new File("data/exercise-03-20-b.dat"),'	')))
  buffer.append(("C", csvread(new File("data/exercise-03-20-c.dat"),'	')))

  // Evaluation of each matrix in the definitions buffer
  for (item <- buffer) {
    results.add(s"Matrix ${item._1}:\n")

    val stdDevs = sqrt(diag(item._2)).map(1 / _)

    val correlations = diag(stdDevs) * item._2 * diag(stdDevs)
    val c = correlations.map("%f".formatLocal(java.util.Locale.US, _))
    results.add(s"Correlations:\n$c\n")

    val determinant = det(correlations)
    val d = Array(determinant).map("%f".formatLocal(java.util.Locale.US, _))
    results.add(s"Determinant: ${d(0)}\n")

    if (determinant == 0.0) results.add(s"Matrix ${item._1} is singular\n")
    else if (determinant <= 0.001)
      results.add(s"Matrix ${item._1} is multicollinear\n")
    else results.add(s"Matrix ${item._1} is not multicollinear\n")
  }

  // Writing the buffered results
  results.all('-', 80)
}
