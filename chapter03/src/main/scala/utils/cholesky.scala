/** This is a simplified version of the ScalaNLP Breeze Cholesky algorithm. The
 *  exception handling has been omitted, in order to use the method for some
 *  positive semi-definite matrices.
 *
 *  The original version was authored by Sergei Lebedev and released under
 *  Apache 2 license. Copyright 2012 David Hall. For further details, please
 *  refer to https://github.com/scalanlp/breeze
 */

package valueatrisk.chapter03

import breeze.generic.UFunc
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance => lapack}
import breeze.linalg._

/** Computes the cholesky decomposition A of the given real symmetric positive
 *  definite matrix X such that X = A A.t. Exception handling is omitted, so
 *  that the method can be used for some positive semi-definite matrices as
 *  well.
 */
object CustomCholesky extends UFunc {
  implicit object ImplCholesky_DM extends Impl[DenseMatrix[Double], DenseMatrix[Double]] {
    def apply(X: DenseMatrix[Double]): DenseMatrix[Double] = {
      val A: DenseMatrix[Double] = lowerTriangular(X)

      val N = X.rows
      val info = new intW(0)
      lapack.dpotrf(
        "L" /* lower triangular */,
        N /* number of rows */,
        A.data,
        scala.math.max(1, N) /* LDA */,
        info
      )
      A
    }
  }
}
