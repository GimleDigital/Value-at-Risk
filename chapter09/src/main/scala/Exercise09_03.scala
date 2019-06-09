package valueatrisk.chapter09

import utils._

import scala.math.{exp}

import breeze.linalg._

/** Proposed solution for exercise 9.3 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise09_03 extends App {
  // Results are buffered and printed later
  val results = new Results("9.3")

  // Exercise definitions
  def theta(r1: Double, r2: Double) = 10000.0 * exp(-0.25 * r2) * (r1 - 76.0)

  def gradient (r1: Double, r2: Double) = DenseMatrix(10000.0 * exp(-0.25 * r2),
      -2500.0 * exp(-0.25 * r2) * (r1 - 76.0))

  val u = DenseMatrix(74.0, 0.041)

  // Function approximation at u
  val thetaU = theta(u(0, 0), u(1, 0))

  results.add("Function approximation at u:")
  results.add(s"${doubleToString(thetaU, 2)}")

  // Gradient approximation at u
  val gradientU = gradient(u(0, 0), u(1, 0))

  results.add("Gradient approximation at u:")
  results.add(s"${gradientU.map(doubleToString(_, 2))}")

  // Linear remapping
  val a = (thetaU - gradientU.t * u).map(doubleToString(_, 1))
  val b = gradientU.map(doubleToString(_, 2))

  results.add("Linear remapping:")
  results.add(s"thetaR = ${a(0, 0)} + ${b(0, 0)} * R1 + ${b(1, 0)} * R2")

  // Printing the results
  results.all('-', 80)

 Thread.sleep(1000)
}
