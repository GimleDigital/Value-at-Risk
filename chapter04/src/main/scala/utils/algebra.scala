package valueatrisk.chapter04

import scala.collection.mutable.ListMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.SortedMap

import breeze.linalg._
import breeze.numerics._

/** Help functions for matrix algebra */
package object algebra {
  /** Returns a matrix where columns with all elements equal to zero have been
   *  removed.
   *
   *  @param matrix the matrix for which the columns shall be removed
   */
  def removeZeroColumns(matrix: DenseMatrix[Double]) = {
    val k = matrix.cols
    val buffer = new ListBuffer[Int]

    for (i <- 0 to k - 1) {
      if (all(matrix(::, 2 - i)) == true) { buffer.append(i) }
    }

    matrix.delete(buffer.toSeq, Axis._1)
  }

  /** Returns the correlation matrix of a covariance matrix
   *
   * @param covariances the covariance matrix
   */
  def covToCorr(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val stdDevs = diag(sqrt(diag(matrix)).map(1 / _))

    stdDevs * matrix * stdDevs
  }

  /** Returns a matrix with the columns in reverse orders
   *
   *  @param the matrix to be reversed
   */
  def reverseCols(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val n = matrix.cols
    val buffer = DenseMatrix.zeros[Double](n,n)

    for (i <- 1 to n) buffer(::, n - i) := matrix(::, i - 1)

    buffer
  }

  /** Returns the eigenvalues of a matrix, ordered from high to low.
   *
   *  @param m the matrix for which the eigenvalues should be calculated
   */
  def eigenValues(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    DenseMatrix(eigSym(m).eigenvalues.toArray.sorted.reverse)
  }

  /** Returns the eigenvectors of a matrix, ordered in correspondence to the
   *  matrix' eigenvalues, ordered from high to low.
   *  @param m the matrix for which the eigenvalues should be calculated
   */
   def eigenVectors(matrix: DenseMatrix[Double]): DenseMatrix[Double]  = {
    val values = eigSym(matrix).eigenvalues
    val vectors = eigSym(matrix).eigenvectors

    val unsorted = new ArrayBuffer[(Double, DenseVector[Double])]
    val n = values.size

    for (i <- 0 to n - 1)
      unsorted.append((utils.rounded(values(i), 10), vectors(::, i)))

    val sorted = ListMap(unsorted.sortWith(_._1 < _._1):_*)

    val iterator = sorted.iterator

    def result(res: DenseMatrix[Double]): DenseMatrix[Double] = {
      println(s"Res:\n$res\n")
      if (iterator.hasNext) {
        result(DenseMatrix.vertcat(res, DenseMatrix(iterator.next._2)))
      }
      else res
    }

    result(DenseMatrix(iterator.next._2))

   }

    /** Returns the approximation of an integral, obtained via Riemann sums.
   *
   *  @param f the function to be evaluated
   *  @param min the start value for the integral
   *  @param max the end value for the integral
   *  @param m the number of steps
   */

  def riemannIntegral(f: Double => Double, min: Double, max: Double,
                      m: Int): Double = {
    val buffer = new ListBuffer[Double]

    val step = (max - min) / m.toDouble

    for ( i <- 1 to m) {
      buffer += f(min + i.toDouble * step) * step
    }

    buffer.reduce(_ + _)
  }

  /** Returns the approximation of an integral, obtained via the trapezoidal
   *  rule.
   *
   *  @param f the function to be evaluated
   *  @param min the start value for the integral
   *  @param max the end value for the integral
   *  @param m the number of steps
   */
  def trapezoidalIntegral(f: Double => Double, min: Double, max: Double,
                      m: Int): Double = {
    val step = (max - min) / m.toDouble

    /** Returns the approximated value, after m iterations
     *
     *  @param iter the remaining iterations
     *  @param prev the accumulated value so far
     *  @param x0 the min value of x
     */
    def trapezoid(iter: Int, prev: Double, x0: Double): Double = {
      if (iter == 0) prev
      else {
        val x1 = x0 + step

        val increment = (f(x0) + f(x1)) * step / 2.0

        trapezoid(iter - 1, prev + increment, x1)
      }
    }

    trapezoid(m, 0.0, min)
  }

  /** Returns the value of x for which the function value is zero, by using
   *  Newton-Raphson method for approximation.
   *
   *  @param f the functions that shall be evaluated
   *  @param j the jacobian parameters, i.e the first order differentials
   *  @param xk the seed values of x, used for the first iteration
   */
  def newtonRaphson(f: DenseMatrix[Double] => DenseMatrix[Double],
                j: DenseMatrix[Double] => DenseMatrix[Double],
                xk: DenseMatrix[Double], iter: Int): DenseMatrix[Double] = {
    val x = xk - inv(j(xk)) * f(xk)
    val y = f(x) + j(x) * (x - xk)

    if (y.map(utils.rounded(_, 5)) ==
      DenseMatrix.zeros[Double](y.rows, y.cols)) x
    else if (iter < 1) {
      println(
        s"\nThe limit of iterations was reached, process stopped.\n")
      DenseMatrix.zeros[Double](xk.rows, xk.cols)
}
    else newtonRaphson(f, j, x, iter - 1) // Not resolved, run next iteration
  }

  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n-1)
  }
}
