package valueatrisk.chapter10

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math._

import breeze.linalg._
import breeze.stats._
import breeze.stats.distributions.MultivariateGaussian

/** Provides utilities for estimating function values for a multivariate normal
 *  distribution.
 *
 *  @param size the sample size to be used
 *  @param u the means of the distributions
 *  @param sigma the covariances of the distribution
 */
class Estimation(size: Int, u: DenseMatrix[Double], sigma: DenseMatrix[Double]) {
  // Multivariate distribution
  val mvg = new MultivariateGaussian(u(::, 0), sigma)

  // Sample
  val draws = new ArrayBuffer[DenseVector[Double]]

  for (i <- 0 to size - 1) draws.append(mvg.draw)

  /** Returns the function values for a sample.
   *
   *  @param f the function to be evaluated
   */
  def getValues (f: DenseMatrix[Double] => DenseMatrix[Double]) = {
    val values = new ArrayBuffer[Double]

    for (draw <- draws) {
      val fx = f(DenseMatrix(draw).t)

      values.append(fx(0, 0))
    }

    values.toArray
  }

  /** Returns a crude Monte Carlo estimator.
   *
   *  @param f the function to be evaluated
   */
  def crudeMonteCarlo(f: DenseMatrix[Double] => DenseMatrix[Double]) = {
    // No need for constructing a Cholesky matrix, Breeze already did it
    val values = getValues(f)

    val mean =  values.sum / size.toDouble

    val diffs = new ArrayBuffer[Double]

    for (value <- values) diffs.append(pow(value - mean, 2))

    sqrt(diffs.sum / size.toDouble)
  }

  /** Returns a Monte Carlo estimator with control variate.
   *
   *  @param f the function to be evaluated
   *  @param cvf the control variate function
   *  @oaram stdP the value of std(P) for the control variate function
   *  @param c the constant (optional)
   */
  def controlVariate(f: DenseMatrix[Double] => DenseMatrix[Double],
                     cvf: DenseMatrix[Double] => DenseMatrix[Double],
                     stdP: Double, c: Double = 1.0) = {
    val valuesF = getValues(f)
    val valuesCVF = getValues(cvf)

    val meanF = valuesF.sum / size.toDouble
    val meanCVF = valuesCVF.sum / size.toDouble

    val diffsF = new ArrayBuffer[Double]
    val diffsCVF = new ArrayBuffer[Double]

    for (i <- 0 to size - 1) {
      diffsF.append(pow(valuesF(i) - meanF, 2))
      diffsCVF.append(pow(valuesCVF(i) - meanCVF, 2))
    }

    sqrt(diffsF.sum / size.toDouble) - c * (sqrt(diffsCVF.sum / size.toDouble)
      - stdP)
  }

  /** Returns a stratified Monte Carlo estimator.
   *
   *  @param f the function to be evaluated
   *  @param ws the stratification size
   *  @param cdf the probabilities and cumulative distribution function values
   */
  def stratifiedMonteCarlo(f: DenseMatrix[Double] => DenseMatrix[Double],
                           ws: Int, cdf: DenseMatrix[Double]) = {

    val intervalLength = (cdf(cdf.rows - 1, 0) - cdf(0, 0)) / (ws - 2.0)
    val probabilities = new Array[Double](ws)
    val sampleSizes = DenseVector.zeros[Int](ws)

    for (j <- 1 to ws) {
      probabilities(j - 1) = j match {
        case 1 => cdf(0, 1)
        case `ws` => 1.0 - cdf(ws - 2, 1)
        case default => cdf(j - 1, 1) - cdf(j - 2, 1)
      }
    }

    /** Returns the sample size for an interval.
     *
     *  @param j the number of the interval
     */
    def sampleSize(j: Int) = {
      val pSum = new ArrayBuffer[Double]

      for (i <- 2 to ws - 1) pSum.append(cdf(i - 1, 1) - cdf(i - 2, 1))

      size * probabilities(j - 1) * (if (j == 1 || j == ws) ws / 3.0 else 1.0) /
        ((cdf(0, 1) + 1.0 - cdf(ws - 2, 1)) * ws / 3.0 + pSum.sum)
    }

    /** Returns a stratified sample of function values by successively obtaining
     *  samples and classifying them in stratas until each strata sample size
     *  has been reached.
     *
     *  @param sizes the sample size of the strata
     *  @param samples the samples
     */
    def getSamples(sizes: DenseVector[Int],
                   samples: ArrayBuffer[DenseVector[Double]]):
                   ArrayBuffer[DenseVector[Double]] = {
      if (sizes.sum == 0) samples
      else {
        val sample = f(DenseMatrix(mvg.draw).t)

        val strata = {
          if (sample(0, 0) > cdf(ws - 2, 0)) ws
          else if (sample(0, 0) < cdf(0, 0)) 1
          else ((sample(0, 0) - cdf(0, 0)) / intervalLength).toInt + 2
        }

        if (sizes(strata - 1) > 0) {
          samples(strata - 1)(sizes(strata - 1) - 1) = sample(0, 0)
          sizes(strata - 1) = sizes(strata - 1) - 1
        }

        getSamples(sizes, samples)
      }
    }

    /** Building the samples */
    val stratifiedSamples = new ArrayBuffer[DenseVector[Double]](ws)

    for (j <- 1 to ws) {
      sampleSizes(j - 1) = rounded(sampleSize(j), 0).toInt

      stratifiedSamples.append(DenseVector.zeros[Double](sampleSizes(j - 1)))
    }

    val sizes = DenseVector.zeros[Int](ws)

    for (i <- 0 to sampleSizes.length - 1) sizes(i) = sampleSizes(i)

    getSamples(sizes, stratifiedSamples)

    /** Monte Carlo estimator */
    val term1 = new ArrayBuffer[Double]
    val term2 = new ArrayBuffer[Double]

    for (i <- 0 to ws - 1) {
      term1.append(probabilities(i) * stratifiedSamples(i).map(pow(_, 2)).sum /
        sampleSizes(i))
      term2.append(probabilities(i) * stratifiedSamples(i).sum / sampleSizes(i))
    }

    sqrt(term1.sum - pow(term2.sum, 2))
  }

  /** Returns a crude Monte Carlo estimation of Value-at-Risk
   *
   *  @param f the function to be evaluated
   *  @param portfolioValue the value of the portfolio
   *  @param level the confidence level
   */
  def crudeMonteCarloVaR(f: DenseMatrix[Double] => DenseMatrix[Double],
                         portfolioValue: Double, level: Double) = {
    val values = getValues(f)
    scala.util.Sorting.quickSort(values)

    portfolioValue - DescriptiveStats.percentile(values.toArray, 1 - level)
  }

  /** Returns a Monte Carlo with control variate estimation of Value-at-Risk.
   *
   *  @param f the function to be evaluated
   *  @param ws the stratification size
   *  @param cdf the probabilities and cumulative distribution function values
   *  @oaram stdP the value of std(P) for the control variate function
   *  @param portfolioValue the value of the portfolio
   *  @param level the confidence level
   *  @param c the constant (optional)
   */
  def controlVariateVaR(f: DenseMatrix[Double] => DenseMatrix[Double],
                        cvf: DenseMatrix[Double] => DenseMatrix[Double],
                        stdP: Double, portfolioValue: Double,
                        level: Double, c: Double = 1.0) = {
    val valuesF = getValues(f)
    val valuesCVF = getValues(cvf)

    scala.util.Sorting.quickSort(valuesF)
    scala.util.Sorting.quickSort(valuesCVF)

    portfolioValue - (DescriptiveStats.percentile(valuesF, 1 - level) - c *
      (DescriptiveStats.percentile(valuesCVF, 1 - level) - stdP))
  }

  /** Returns a stratified Monte Carlo estimation of Value-at-Risk.
   *
   *  @param f the function to be evaluated
   *  @param cdf the probabilities and cumulative distribution function values
   *  @oaram stdP the value of std(P) for the control variate function
   *  @param portfolioValue the value of the portfolio
   */
  def stratifiedMonteCarloVaR(f: DenseMatrix[Double] => DenseMatrix[Double],
                               stdP: Double, portfolioValue: Double,
                               level: Double) = {
    val size0 = rounded((1.0 - level) * size, 0).toInt
    val size1 = size - size0

    val sizes = Array(size0, size1)

    def getSamples(sizes: Array[Int], samples: Array[DenseVector[Double]]):
                   Array[DenseVector[Double]] = {
      if (sizes.sum == 0) samples
      else {
        val sample = f(DenseMatrix(mvg.draw).t)

        val strata = {
          if (sample(0, 0) <= stdP) 0
          else 1
        }

        if (sizes(strata) > 0) {
          samples(strata)(sizes(strata) - 1) = sample(0, 0)
          sizes(strata) = sizes(strata) - 1
        }

        getSamples(sizes, samples)
      }
    }

    /** Building the samples */
    val stratifiedSamples = Array(DenseVector.zeros[Double](size0),
      DenseVector.zeros[Double](size1))

    getSamples(sizes, stratifiedSamples)

    /** Monte Carlo estimator */
    val buffer = new ArrayBuffer[Double]
    for (i <- 0 to size0 - 1) buffer.append(stratifiedSamples(0)(i))
    for (i <- 0 to size1 - 1) buffer.append(stratifiedSamples(1)(i))

    val values = buffer.toArray

    scala.util.Sorting.quickSort(values)

    portfolioValue - DescriptiveStats.percentile(values, 1 - level)
  }
}