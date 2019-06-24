package valueatrisk.chapter10

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, Pi, exp, pow, sin, sqrt}

import java.io._

import breeze.linalg._
import breeze.math._
import breeze.stats.distributions.ChiSquared

/** Proposed solution for exercise 10.2 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise10_02 extends App {
  // Results are buffered and printed later
  val results = new Results("10.2")

  // Exercise definitions
  val currentValue = 53600.0
  def p(r1: Double) = 80.0 * pow(r1, 2) + 1120.0 * r1 + 76080.0

  // a) Quadratic polynomial
  def rDot(r1: Double) = 4.0 * r1 + 25

  results.add("a) Quadratic polynomial")
  results.add(s"c: ${-80.0 * 16.0}")
  results.add(s"b: ${-80.0 * 2.0 * 4.0 * 25.0 + 1120.0 * 4.0}")
  results.add(s"a: ${-80.0 * pow(25.0, 2) + 1120.0 * 25.0 + 76080.0}")

  // c) Conditional values
  val u = DenseMatrix(25.0, 0.0, 0.0).t

  val c = DenseMatrix((-80.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0))
  val b = DenseMatrix(1120, 0.0, 0.0)
  val a = DenseMatrix(76080.0)

  val cDot = DenseMatrix((-1280.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0))
  val bDot = DenseMatrix(-11520, 0.0, 0.0)
  val aDot = DenseMatrix(54080.0)

  val moments = new Moments(u, c, b, a, cDot, bDot, aDot)

  val g = new Array[Double](6)

  g(0) = moments.getG(0)
  for (k <- 1 to 4) {
    g(k) = moments.getG(k)
  }

  results.add("c) Conditional values")
  results.add("k  g")
  results.add("-  ---------------------")
  for (k <- 0 to 4) results.add(s"$k  ${g(k)}")

  // d) Moments

  val eP = new Array[Double](6)

  for (k <- 1 to 4) {
    eP(k) = moments.getEY(k)
  }
  eP(5) = moments.getEY(5)

  results.add("d) Moments")

  results.add("k  E(P)")
  results.add("-  ---------------------")
  for (k <- 1 to 5) results.add(s"$k  ${eP(k)}")

  // e) Standard deviation of portfolio value
  val stdL = sqrt(moments.getEY(2) - pow(moments.getEY(1), 2))

  results.add("e) Standard deviation of portfolio value")
  results.add(s"std(L): ${doubleToString(stdL, 0)}")

  // f) Conditional central moments
  val mean = g(0)
  val stdDev = stdL

  results.add("f) Conditional central moments")

  val cM = new ArrayBuffer[Double]

  results.add("Central moments:")

  for (k <- 1 to 5) {
    cM.append(moments.getCM(k))

    results.add(s"${cM(k - 1)}")
  }

  val nCM = new ArrayBuffer[Double]

  results.add("Normalized central moments:")

  for (k <- 1 to 5) {
    nCM.append (cM(k - 1) / pow(stdDev, k))

    results.add(s"${doubleToString(nCM(k - 1), 3)}")
  }

  val cumulants = new Array[Double](5)

  cumulants(0) = 0.0 // Normalized mean
  cumulants(1) = 1.0  // Normalized standard deviation
  cumulants(2) = nCM(2)
  cumulants(3) = nCM(3) - 3.0 * pow(cumulants(1), 2)
  cumulants(4) = nCM(4) - 10.0 * cumulants(2) * cumulants(1)

  results.add("Cumulants:")
  for (cum <- cumulants) results.add(s"${doubleToString(cum, 3)}")

  // g) Cornish-Fischer expansion
  val quantile = 0.10

  val cornishFisher = new CornishFisher(quantile, mean, stdDev, g)

  val components = new Array[Double](7)
  for (k <- 1 to 7) components(k -1) = cornishFisher.getComponent(k)

  val normalizedP = components.sum
  val quantileP = normalizedP * stdDev + mean
  val quantileLoss = currentValue - quantileP

  results.add("g) Cornish-Fischer expansion")
  results.add(s"0.10 quantile of P*:	${doubleToString(normalizedP, 4)}")
  results.add(s"0.10 quantile of 1P:	${doubleToString(quantileP, 0)}")
  results.add(s"0.90 Value-at-Risk:	${doubleToString(quantileLoss, 0)}")

  // h) Completing the squares
  val alpha = 80000.0
  val beta = 0.0
  val gamma = Array(-1280.0)
  val variance = Array(pow(4.5, 2))

  // k) Characteristic function
  val nccs = new NonCentralChiSquare(alpha, beta, gamma, variance)

  val uUpper = 0.0005
  val intervals = 100
  val price0 = 25000.0
  val price1 = 50000.0
  val q = 0.10

  results.add("k) Characteristic function")

  val estimatedP = nccs.pValue(q, uUpper, intervals, price0, price1, 1, 100)

  results.add(s"0.10-quantile of P:	${doubleToString(estimatedP, 0)}")
  results.add(
    s"0.90 Value-at-Risk:	${doubleToString(currentValue - estimatedP, 0)}")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
