package valueatrisk.chapter10

import utils._

import scala.collection.mutable.ArrayBuffer

import java.io._

import breeze.linalg._
import breeze.numerics._

/** Proposed solution for exercise 10.3 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise10_03 extends App {
  // Results are buffered and printed later
  val results = new Results("10.3")

  // Exercise definitions
  val r = DenseVector(87.0, 94.0, 26.0, 0.89)
  val w = DenseVector(1000.0, 3000.0, -2000.0)
  val sigma = csvread(new File("data/exercise-10-03-sample.dat"), ';')

  // a) Portfolio value

  val p = (w :* r(0 to 2)).sum * r(3)

  results.add("a) Portfolio value")
  results.add(s"P(0): ${doubleToString(p, 0)} EUR")

  // c) Mapping
  val c = DenseMatrix((0.0, 0.0, 0.0, 500.0), (0.0, 0.0, 0.0, 1500.0),
                      (0.0, 0.0, 0.0, -1000.0), (500.0, 1500.0, -1000.0, 0.0))
  val b = DenseMatrix(0.0, 0.0, 0.0, 0.0)
  val a = DenseMatrix(0.0)

  // d) Cholesky matrix
  val z = cholesky.ImplCholesky_DM(sigma)

  results.add("d) Cholesky matrix")
  results.add(s"${z.map(doubleToString(_, 5))}")

  // e) The z'cz matrix and its eigenvectors
  val z2 = DenseMatrix((0.43500, 0.0, 0.0, 0.0),
                       (0.15134, 0.64036, 0.0, 0.0),
                       (0.01248, 0.03338, 0.20492, 0.0),
                       (0.00039, -0.00080, 0.00120, 0.00967))

  val zcz = z.t * c * z

  results.add("e) The z'cz matrix")

  results.add("z'cz matrix:")
  results.add(s"${zcz.map(doubleToString(_, 5))}")

  val e = eigSym(zcz).eigenvectors

  val u = e.t // Breeze generates normalized ortogonal eigenvectors

  results.add("u:")
  results.add(s"${u.map(doubleToString(_, 4))}")

  // f) Mapping R ~ N(0, I)
  val m = z * inv(u)

  results.add("f) Mapping R ~ N(0, I)")
  results.add(s"${m.map(doubleToString(_, 4))}")

  // g) Mapping P = f(R)
  val my = DenseMatrix(r).t

  val cDot = u * z.t * c * z * u.t
  val bDot = (2.0 * my.t * c + b.t) * z * u.t
  val aDot = my.t * c * my + b.t * my + a

  results.add("g) Mapping P = θ φ(R)")
  results.add("cDot:")
  results.add(s"${cDot.map(doubleToString(_, 4))}")
  results.add("bDot:")
  results.add(s"${bDot.map(doubleToString(_, 2))}")
  results.add("aDot:")
  results.add(s"${aDot.map(doubleToString(_, 0))}")

  // h) Conditional values g
  val moments = new Moments(my.t, c, b, a, cDot, bDot.t, aDot)

  val g = new ArrayBuffer[Double]
  val eP = new ArrayBuffer[Double]

  results.add("h) Conditional values of g")
  results.add("k  g(k)")
  results.add("-  ----------------")

  for (k <- 0 to 4) {
    g.append(moments.getG(k))

    results.add(s"$k  ${g(k)}")
  }

  results.add("i) Conditional moments E(P)")
  results.add("k  E(P)")
  results.add("-  ----------------")

  for (k <- 1 to 5) {
    eP.append(moments.getEY(k))

    results.add(s"$k  ${eP(k - 1)}")
  }

  // j) Standard deviation of loss
  val stdL = sqrt(moments.getEY(2) - pow(moments.getEY(1), 2))

  results.add("j) Standard deviation of loss")
  results.add(s"Std(L): ${doubleToString(stdL, 0)}")

  // k) Normalized moments and cumulants
  val mean = g(0)
  val stdDev = stdL

  val cM = new Array[Double](5)
  val nCM = new ArrayBuffer[Double]
  val nCum = new Array[Double](5)

  results.add("k) Moments and cumulants")

  results.add("Central moments:")

  cM(0) = 0.0
  cM(1) = eP(1) - pow(eP(0), 2)
  cM(2) = eP(2) - 3.0 * eP(0) * eP(1) + 2.0 * pow(eP(0), 3)
  cM(3) = eP(3) - 4.0 * eP(2) * eP(0) + 6.0 * eP(1) * pow(eP(0), 2) - 3.0 * pow(eP(0), 4)
  cM(4) = eP(4) - 5.0 * eP(3) * eP(0) + 10.0 * eP(2) * pow(eP(0), 2) - 10.0 * eP(1) * pow(eP(0), 3) + 4.0 * pow(eP(0), 5)


  for (k <- 1 to 5) {
    results.add(s"${cM(k - 1)}")
  }

  results.add("Normalized central moments:")

  for (k <- 1 to 5) {
    nCM.append (cM(k - 1) / pow(stdDev, k))

    results.add(s"${doubleToString(nCM(k - 1), 4)}")
  }

  nCum(0) = 0.0 // Normalized mean
  nCum(1) = 1.0  // Normalized standard deviation
  nCum(2) = nCM(2)
  nCum(3) = nCM(3) - 3.0 * pow(nCum(1), 2)
  nCum(4) = nCM(4) - 10.0 * nCum(2) * nCum(1)

  results.add("Normalized cumulants:")
  for (cum <- nCum) results.add(s"${doubleToString(cum, 6)}")

  // l) Cornish-Fisher expansion
  val q = 0.05 // Quantile
  val cornishFisher = new CornishFisher(q, mean, stdDev, g.toArray)
  val cF = new ArrayBuffer[Double]

  results.add("l) Cornish-Fisher expansion")

  for (k <- 1 to 7) {
    cF.append(cornishFisher.getComponent(k))
  }

  results.add(s"Quantile - Normalization of 1P: ${doubleToString(cF.sum, 4)}")
  results.add(s"quantile - 1P: ${doubleToString(cF.sum * stdDev + mean, 0)}")

  // m) Completing the squares
  val alpha = 0.0
  val beta = 0.0
  val gamma = Array(-11.0281, 9.3977)
  val variance = Array(pow(41.5276, 2), pow(179.0108, 2))

  // n) Characteristic function
  val nccs = new NonCentralChiSquare(alpha, beta, gamma, variance)

  val uUpper = 0.0005
  val intervals = 100
  val price0 = 280000.0
  val price1 = 290000.0

  results.add("k) Characteristic function")

  val estimatedP = nccs.pValue(q, uUpper, intervals, price0, price1, 1, 100)

  results.add(s"0.05-quantile of P: ${doubleToString(estimatedP, 0)}")
  results.add(
    s"0.95 Value-at-Risk:   ${doubleToString(p - estimatedP, 0)}")
  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
