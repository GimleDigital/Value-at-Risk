package valueatrisk.chapter09

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.{exp, pow}

import java.io.File

import breeze.linalg._

/** Proposed solution for exercise 9.6 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise09_06 extends App {
  // Results are buffered and printed later
  val results = new Results("9.6")

  // Exercise definitions
  val alpha1 = 0.02
  val alpha2 = 0.03

  val sample = csvread(new File("data/exercise-09-06-sample.dat"), '	')

  /** Returns the "forces" affecting a sample point, i.e. the impact on it by
   *  all other sample points.
   *
   *  @param k the sample point
   *  @param j the current other sample point
   *  @param acc the accumulated sum of forces so far
   */
  def force(p: DenseMatrix[Double], k: Int, j: Int, acc: DenseVector[Double]):
    DenseVector[Double] = {
    if (j > p.cols - 1) acc
    else if (j == k) force(p, k, j + 1, acc)
    else {
      val v = (p(::, k) - p(::, j))

      force(p, k, j + 1, acc + alpha2 * v / pow(norm(v), 2))
    }
  }

  /** Returns the "shifted" values of the sample points.
   *
   *  @param p the previous matrix of observation points
   *  @param f the forces to be applied on the previous  points
   */
  def shift(p: DenseVector[Double], f: DenseVector[Double]) =
    (p + f) / norm(p + f)

  def evalCondition(pAct: DenseMatrix[Double], pAnt: DenseMatrix[Double]):
    Boolean = {
    val arr = new ArrayBuffer[Double]

    for (i <- 0 to pAct.cols - 1)
      arr.append(norm(pAct(::, i) - pAnt(::, i)))

    arr.max < alpha1
  }

  /** Returns the  minimum energy configuration.
   *  @param p0 the matrix of initial observation points
   */
  def findMinimum(p0: DenseMatrix[Double]): DenseMatrix[Double] = {
    val f0 = DenseMatrix.zeros[Double](p0.rows, p0.cols)
    for (j <- 0 to f0.cols - 1) {
      val f = force(p0, j, 0, DenseVector.zeros[Double](p0.rows))

      for (i <- 0 to f0.rows - 1) f0(i, j) = f(i)
    }

    val pk = DenseMatrix.zeros[Double](p0.rows, p0.cols)

    for (j <- 0 to pk.cols - 1) {
      val s = shift(p0(::, j), f0(::, j))

      for (i <- 0 to pk.rows - 1) pk(i, j) = s(i)
    }

    if (evalCondition(p0, pk) == true) pk
    else findMinimum(pk)
  }
  def minimum = findMinimum(sample)

  results.add(s"${minimum.t.map(doubleToString(_, 5))}")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
