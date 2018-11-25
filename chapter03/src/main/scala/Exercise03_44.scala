package valueatrisk.chapter03

import java.io.File

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 3.44 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise03_44 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("3.44")

  // Exercise definitions
  val gk = csvread(new File("data/exercise-03-44-gk.dat"), '	')
  val ur = csvread(new File("data/exercise-03-44-ur.dat"), '	')

  val quantile = 0.10

  // Mean and standard deviation
  val mean = gk(0, 1)
  val stdDev = sqrt(ur(1, 1))

  results.add(
    s"Mean = $mean, standard deviation = ${utils.rounded(stdDev, 5)}\n")

  // Cumulants of Y*
  val cumYs = DenseMatrix(ur(::, 3)).t

  results.add(s"Cumulants of Y*:\n${cumYs.t}\n")

  // Cornish-Fisher Expansion Coefficients
  val normalDistribution = new statistics.NormalDistribution(0, 1)

  val cfec1 = normalDistribution.inverseCdf(quantile)
  val cfec2 = (pow(cfec1, 2) - 1.0) * 1 / 6.0
  val cfec3 = (pow(cfec1, 3) - 3.0 * cfec1) / 24.0
  val cfec4 = -(2.0 * pow(cfec1, 3) - 5.0 * cfec1) / 36.0
  val cfec5 = (pow(cfec1, 4) - 6.0 * pow(cfec1, 2) + 3.0) / 120.0
  val cfec6 = -(pow(cfec1, 4) - 5.0 * pow(cfec1, 2) + 2.0) / 24.0
  val cfec7 = (12.0 * pow(cfec1, 4) - 53 * pow(cfec1, 2) + 17.0) / 324.0

  val cfec = DenseMatrix(cfec1, cfec2, cfec3, cfec4, cfec5, cfec6, cfec7)

  results.add(s"Cornish-Fisher Expansion Coefficients:")
  results.add(s"${cfec.t.map(utils.rounded(_, 5))}\n")

  // Cornish-Fisher Expansion Cumulant Terms
  val cfect1 = cumYs(1, 0)
  val cfect2 = cumYs(2, 0)
  val cfect3 = cumYs(3, 0)
  val cfect4 = pow(cumYs(2, 0), 2)
  val cfect5 = cumYs(4, 0)
  val cfect6 = cumYs(2, 0) * cumYs(3, 0)
  val cfect7 = pow(cumYs(2, 0), 3)

  val cfect = DenseMatrix(cfect1, cfect2, cfect3, cfect4,
                          cfect5, cfect6, cfect7)

  results.add(s"Cornish-Fisher Expansion Cumulant Terms:")
  results.add(s"${cfect.t.map(utils.rounded(_, 5))}\n")

  // Cornish-Fisher Expansion Terms
  val cft = cfec :* cfect

  results.add(s"Cornish-Fisher Terms:\n${cft.t.map(utils.rounded(_, 5))}\n")

  // Quantiles
  val qYs = sum(cft)
  val qY = stdDev * qYs + mean

  results.add(s"Quantile of Y*: ${utils.rounded(qYs, 5)}")
  results.add(s"Quantile of Y:  ${utils.rounded(qY, 5)}\n")

  // Writing the buffered results
  results.all('-', 80)
}
