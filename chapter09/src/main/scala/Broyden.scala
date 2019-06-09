package valueatrisk.chapter09

import utils._

import breeze.linalg.{DenseMatrix, inv}

/** Provides a basic implementation of the Broyden method for finding the roots
 *  of a non-linear equation system, similar to the one-dimensional tangent
 *  method.
 *
 *  @param maxIterations the maximum number of iterations (a safety measure)
 */
case class Broyden(maxIterations: Int = 100) {
  /** Returns the values of n functions, evaluated for specific values of n
   *  variables.
   *
   *  @param f the array function to be evaluated
   *  @param a0 the approximation of the Jacobian matrix in the previous step
   *  @param xk the new values to be tested
   *  @param iter the number of iterations so far
   */
  def approximate(f: DenseMatrix[Double] => DenseMatrix[Double],
                  a0: DenseMatrix[Double],
                  xk: DenseMatrix[Double], iter: Int): DenseMatrix[Double] = {
    val x = xk - inv(a0) * f(xk)

    val dk = x - xk

    val a = (a0 + (f(x) - f(xk) - a0 * dk) * dk.t / (dk * dk.t))

    val y = f(x) + a * (x - xk)

    if (y.map(rounded(_, 4)) == DenseMatrix.zeros[Double](y.rows, y.cols)) x
    else if (iter >= maxIterations) {
      println(
        s"The limit of $maxIterations iterations was reached, process stopped.")
      DenseMatrix.zeros[Double](xk.rows, xk.cols)
    }
    else {
      approximate(f, a, x, iter + 1) // Not resolved, run next iteration
    }
  }
}
