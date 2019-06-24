package valueatrisk.chapter10

import utils._

import scala.collection.mutable.ArrayBuffer

import java.io._

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

/** Proposed solution for exercise 10.5 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise10_05 extends App {
  // Results are buffered and printed later
  val results = new Results("10.5")

  // Exercise definitions
  val sample = csvread(new File("data/exercise-10-05-sample.dat"), ';')

  val mean = DenseMatrix(87.0, 94.0, 26.0, 0.89).t

  val sigma = csvread(new File("data/exercise-10-05-sigma.dat"), ';')

  val w = DenseMatrix(1000.0, 3000.0, -2000.0)

  val p = w.t * mean(0, 0 to 2).t * mean(0, 3)

  // Cholesky matrix for covariances
  val z = cholesky.ImplCholesky_DM(sigma)

  // Independent N(0,1) random variables

  def normalValues(n: Double) = sqrt(2.0) * erfinv(2 * n - 1)

  val normal = sample.map(normalValues(_))

  // Realizations of R
  val r = new ArrayBuffer[DenseMatrix[Double]]

  for (i <- 0 to sample.rows - 1) {
    val ri = DenseMatrix(normal(i, ::).t)

    r.append((z * ri.t).t + mean)
  }

  // Portfolio realizations
  val pr_calc = new ArrayBuffer[Double]

  for (i <- 0 to sample.rows - 1) {
    val ri = DenseMatrix(r(i)(0, 0 to 2).t)

    val pv = ri * w * r(i)(0, 3)

    pr_calc.append(pv(0, 0))
  }

  // Realizations as of Excel solution (to compare and check results)
  val pr_file = csvread(new File("data/exercise-10-05-value.dat"), ';')

  val pr = pr_calc
  // val pr = pr_file

  // Standard deviations
  val dv100 = DenseVector(pr.toArray)(0 to 99)
  val mean100 = dv100.sum / dv100.length
  val stdDev100 = sqrt(dv100.map(_ - mean100).map(pow(_, 2)).sum /
    (dv100.length - 1))

  val dv1000 = DenseVector(pr.toArray)(0 to 999)
  val mean1000 = dv1000.sum / dv1000.length
  val stdDev1000 = sqrt(dv1000.map(_ - mean1000).map(pow(_, 2)).sum /
    (dv1000.length - 1))

  val dv10000 = DenseVector(pr.toArray)(0 to 9999)
  val mean10000 = dv10000.sum / dv10000.length
  val stdDev10000 = sqrt(dv10000.map(_ - mean10000).map(pow(_, 2)).sum /
    (dv10000.length - 1))

  results.add("Standard deviations")
  results.add(s"Sample size = 10000: ${doubleToString(stdDev10000, 0)}")
  results.add(s"Sample size = 1000:  ${doubleToString(stdDev1000, 0)}")
  results.add(s"Sample size = 100:   ${doubleToString(stdDev100, 0)}")

  // Value-at-Risk

  val var_calc = Array(p - DescriptiveStats.percentile(dv10000.toArray, 0.10),
                       p - DescriptiveStats.percentile(dv1000.toArray, 0.10),
                       p - DescriptiveStats.percentile(dv100.toArray, 0.10))

  // Percentiles as of Excel solution (to compare and check results)
  val arr10000 = dv10000.toArray
  scala.util.Sorting.quickSort(arr10000)
  val arr1000 = dv1000.toArray
  scala.util.Sorting.quickSort(arr1000)
  val arr100 = dv100.toArray
  scala.util.Sorting.quickSort(arr100)

  val var_file = Array(p(0) - 0.099 * arr10000(999) - (0.901) * arr10000(1000),
                       p(0) - 0.09 * arr1000(99) - (0.91) * arr1000(100),
                       p(0) - 0.1 * arr100(9) - (0.9) * arr100(10))

  val valueAtRisk = var_calc.map(_(0))
  // val valueAtRisk = var_file

  results.add("Value-at-Risk")

  results.add(s"Sample size = 10000: ${doubleToString(valueAtRisk(0), 0)}")
  results.add(s"Sample size = 1000:  ${doubleToString(valueAtRisk(1), 0)}")
  results.add(s"Sample size = 100:   ${doubleToString(valueAtRisk(2), 0)}")

  // Printing the results
  results.all('-', 80)

  Thread.sleep(1000)
}
