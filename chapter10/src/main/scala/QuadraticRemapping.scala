package valueatrisk.chapter10

import scala.math.pow

import breeze.linalg._

/** Provides some utilities for quadratic remapping.
 *
 *  @param x the m * 1 matrix of variable means
 *  @param y the n * 1 matrix of results
 */
 class QuadraticRemapping(x: DenseMatrix[Double], y: DenseMatrix[Double]) {
   val xSize = x.rows
   val ySize = y.rows

  /** Returns the number of unique pairwise combinations for a set of items.
    *
    *  @param n the number of items
    *  @param a the items examined so far
    *  @param b the combinations found so far
    */
  def combinations(n: Int, a: Int, b: Int): Int = {
    if (a >= n) b
    else (combinations(n, a + 1, a + b))
  }

  /** Returns a matrix with values for all squared xi, all unique combinations
   *  of xi * xj (i != j), all xi and a unit parameter (1.0); and with rows for
   *  all values of y.
   */
  def getF = {
    val comb = combinations(xSize, 1, 0)

    val f = DenseMatrix.zeros[Double](ySize, xSize + comb + xSize + 1)

    for (i <- 0 to ySize - 1) {
      for (j <- 0 to xSize - 1) f(i, j) = pow(x(j, i), 2)

      for (j <- 0 to xSize - 1) {
        for (k <- 0 to xSize - 1)
          if (j < k) f(i, xSize + j) = 2.0 * x(j, i) * x(k, i)

        f(i, j + xSize + comb) = x(j, i)

        f(i, xSize + comb + xSize) = 1.0
      }
    }

    f
  }
}
