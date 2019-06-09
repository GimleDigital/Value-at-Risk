package valueatrisk.chapter09

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, exp, log, pow, sqrt}

import java.io._

import breeze.linalg._
import breeze.stats.distributions.Gaussian

/** Proposed solution for exercise 9.2 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise09_02 extends App {
  // Results are buffered and printed later
  val results = new Results("9.2")

  // Exercise definitions
  val contracts = csvread(new File("data/exercise-09-02-sample.dat"), '	')
    .map(_.toDouble)

  val s = 1.0/0.6414 // Current CAD/USD exchange rate

  // a) Current values for CAD interest rates
  val aCAD = DenseMatrix((0.25, 1.0), (0.50, 1.0))
  val yCAD = DenseMatrix(0.0393, 0.0385)

  val bCAD = inv(aCAD) * yCAD

  results.add("a) Current values for CAD interest rates")
  results.add(s"rCAD = ${doubleToString(bCAD(0, 0), 4)}y + ${doubleToString(bCAD(1, 0), 4)}")

  // b) Current values for USD interest rates
  val aUSD = DenseMatrix((0.25, 1.0), (0.50, 1.0))
  val yUSD = DenseMatrix(0.0352, 0.0354)

  val bUSD = inv(aUSD) * yUSD

  results.add("b) Current values for USD interest rates")
  results.add(s"rUSD = ${doubleToString(bUSD(0, 0), 4)}y + ${doubleToString(bUSD(1, 0), 4)}")

  // c) Implied volatilities

  val strike1 = 1.550
  val strike2 = 1.575
  val expiry1 = 0.25
  val expiry2 = 0.50

  val yVol = DenseMatrix(0.079, 0.074, 0.071, 0.068)

  val aVol = DenseMatrix((strike1 * expiry1, strike1, expiry1, 1.0),
                         (strike2 * expiry1, strike2, expiry1, 1.0),
                         (strike1 * expiry2, strike1, expiry2, 1.0),
                         (strike2 * expiry2, strike2, expiry2, 1.0))

  val bVol = inv(aVol) * yVol

  val bVol0 = doubleToString(bVol(0,0), 3)
  val bVol1 = s"${getSign(bVol(1,0))} ${doubleToString(abs(bVol(1, 0)), 3)}"
  val bVol2 = s"${getSign(bVol(2,0))} ${doubleToString(abs(bVol(2, 0)), 3)}"
  val bVol3 = s"${getSign(bVol(3,0))} ${doubleToString(abs(bVol(3, 0)), 3)}"

  results.add("c) Implied volatilities")
  results.add(s"${bVol0}xy ${bVol1}x ${bVol2}y ${bVol3}")

  // d) Formulas for option's value, delta and vega
  def r1(y: Double): Double = bCAD(0, 0) * y + bCAD(1, 0)
  def r2(y: Double): Double = bUSD(0, 0) * y + bUSD(1, 0)
  def v(x: Double, y: Double): Double = bVol(0, 0) * x * y + bVol(1, 0) * x +
                                     bVol(2, 0) * y + bVol(3, 0)

  val valuation = new Valuation(s)

  // e) Option prices, deltas, and vegas
  val options = new ArrayBuffer[Option]
  val prices = new ArrayBuffer[Double]
  val deltas = new ArrayBuffer[Double]
  val vegas = new ArrayBuffer[Double]

  for (i <- 0 to contracts.rows - 1) {
    val opt = Option(contracts(i, 1), contracts(i, 0), contracts(i, 2), r1, r2, v)

    options.append(opt)

    prices.append(valuation.getPrice(opt))
    deltas.append(valuation.getDelta(opt))
    vegas.append(valuation.getVega(opt))
  }

  results.add("e) Option prices, deltas, and vegas")
  results.add("Price	Delta	Vega")
  results.add("-----	-----	-----")

  for (i <- 0 to options.length - 1) {
    val price = doubleToString(prices(i), 3)
    val delta = doubleToString(deltas(i), 3)
    val vega = doubleToString(vegas(i), 3)

    results.add(s"$price	$delta	$vega")
  }

  results.add("-----	-----	-----")

  val sums = Array(prices.sum, deltas.sum, vegas.sum).map(doubleToString(_, 3))

  results.add(s"${sums(0)}	${sums(1)}	${sums(2)}")

  // f) Weighted strike and exercise averages
  val notionals = new ArrayBuffer[Double]

  for (option <- options) notionals.append(option.n)

  val weightedStrikes = new ArrayBuffer[Double]
  val weightedExpiries = new ArrayBuffer[Double]

  for (i <- 0 to options.length - 1) {
    weightedStrikes.append(options(i).x * prices(i))
    weightedExpiries.append(options(i).y * prices(i))
  }

  results.add("f) Weighted average strikes and expiries")

  results.add("Strike	Amount	Expiry")
  results.add("------	------	------")

  for (i <- 0 to options.length - 1) {
    val ws = doubleToString(weightedStrikes(i), 4)
    val we = doubleToString(weightedExpiries(i), 4)

    results.add(s"$ws	${doubleToString(notionals(i), 0)}	$we")
  }

  results.add("------	------	------")

  val sumStrikes = doubleToString(weightedStrikes.sum / prices.sum, 4)
  val sumExpiries = doubleToString(weightedExpiries.sum / prices.sum, 4)
  val sumNotionals = doubleToString(notionals.sum, 0)

  results.add(s"$sumStrikes	$sumNotionals	$sumExpiries")

  // g) Single option that matches total value, delta and vega
  val g = new Gaussian(0, 1)

  def f(m: DenseMatrix[Double]): DenseMatrix[Double]  = {
    val option = new Option(m(1, 0), m(0, 0), m(2, 0), r1, r2, v)

    val price = valuation.getPrice(option)
    val delta = valuation.getDelta(option)
    val vega = valuation.getVega(option)

    DenseMatrix(pow(price - prices.sum, 2), pow(delta - deltas.sum, 2), pow(vega - vegas.sum, 2))
  }

  val x0 = DenseMatrix(1.5583, 100, 0.3525)
  val xk = DenseMatrix(1.5590, 99, 0.3500)

  val x1 = DenseMatrix(xk(0, 0), x0(1, 0), x0(2, 0))
  val x2 = DenseMatrix(x0(0, 0), xk(1, 0), x0(2, 0))
  val x3 = DenseMatrix(x0(0, 0), x0(1, 0), xk(2, 0))

  val a11 = (f(x1)(0, 0) - f(x0)(0, 0)) / (x1(0, 0) - x0(0, 0))
  val a12 = (f(x2)(0, 0) - f(x0)(0, 0)) / (x2(1, 0) - x0(1, 0))
  val a13 = (f(x3)(0, 0) - f(x0)(0, 0)) / (x3(2, 0) - x0(2, 0))
  val a21 = (f(x1)(1, 0) - f(x0)(1, 0)) / (x1(0, 0) - x0(0, 0))
  val a22 = (f(x2)(1, 0) - f(x0)(1, 0)) / (x2(1, 0) - x0(1, 0))
  val a23 = (f(x3)(1, 0) - f(x0)(1, 0)) / (x3(2, 0) - x0(2, 0))
  val a31 = (f(x1)(2, 0) - f(x0)(2, 0)) / (x1(0, 0) - x0(0, 0))
  val a32 = (f(x2)(2, 0) - f(x0)(2, 0)) / (x2(1, 0) - x0(1, 0))
  val a33 = (f(x3)(2, 0) - f(x0)(2, 0)) / (x3(2, 0) - x0(2, 0))

  val a0 = DenseMatrix((a11, a12, a13), (a21, a22, a23), (a31, a32, a33))

  val broyden = new Broyden // Newton-Raphson would require a true Jacobian

  val approx = broyden.approximate(f, a0, xk, 1)

  results.add("g) Single option that matches total value, delta and vega")

  results.add(s"Strike:     ${doubleToString(approx(0, 0), 4)} CAD/USD")
  results.add(s"Notional:   ${doubleToString(approx(1, 0), 2)} MM USD")
  results.add(s"Expiry:     ${doubleToString(approx(2, 0), 4)} years")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
