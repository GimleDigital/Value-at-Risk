package valueatrisk.chapter02

import scala.math.pow

import breeze.integrate._

/** Proposed solution for exercise 2.22 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_22 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.22")
  
  // Min and max values of x
  val x0 = 1
  val x1 = 2
  
  // Number of iterations
  val iter = 2000
  
  // Evaluating the original integral by using Simpson's rule
  def originalFunction = (x: Double) => x / (5 * pow(x, 2) + 1)
  
  val originalIntegral = simpson(originalFunction, x0, x1, iter)
  
  results.add(s"Original integral: = $originalIntegral\n")

  // Evaluating the integral after change of variables
  def inverseFunction(x: Double) = 5 * pow(x, 2) + 1 // The inverse of g(u)
  
  val u0 = inverseFunction(x0)
  val u1 = inverseFunction(x1)
  
  def changedFunction = (u: Double) => 1 / (10 * u)
  
  val changedIntegral = simpson(changedFunction, u0, u1, iter)
  
  results.add(s"Changed integral: = $changedIntegral\n")

  // Writing the buffered results
  results.all('-', 80) 
}
