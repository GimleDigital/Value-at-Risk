package valueatrisk.chapter02

import scala.math.{exp, pow}

import breeze.linalg._

/** Proposed solution for exercise 2.21 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_21 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.21")

  /** Returns function 1's value for given values of x1 and x2.
   *
   *  @param x1 the value of x1
   *  @param x2 the value of x2
   */
  def f1(x1: Double, x2: Double) = pow(x1, 2) * pow(x2, 3) - x1 * pow(x2, 3) -1

  /** Returns function 1's value for given values of x1 and x2.
   *
   *  @param x1 the value of x1
   *  @param x2 the value of x2
   */
  def f2(x1: Double, x2: Double) = pow(x1, 3) - x1 * pow(x2, 3) -4

  /** Returns function 2's value for given values of x1 and x2.
   *
   *  @param x1 the value of x1
   *  @param x2 the value of x2
   */
  def d11(x1: Double, x2: Double) = 2 * x1 * pow (x2, 3) - pow (x2, 3)

  /** Returns function 1's first order differential with respect to x2 for given
   *  given values of x1 and x2.
   *
   *  @param x1 the value of x1
   *  @param x2 the value of x2
   */
  def d12(x1: Double, x2: Double) =
    3 * pow(x1, 2) * pow(x2, 2) - 3 * x1 * pow(x2, 2)

  /** Returns function 2's first order differential with respect to x1 for given
   *  given values of x1 and x2.
   *
   *  @param x1 the value of x1
   *  @param x2 the value of x2
   */
  def d21(x1: Double, x2: Double) = 3 * pow(x1, 2) - pow(x2, 3)

  /** Returns function 2's first order differential with respect to x2 for given
   *  given values of x1 and x2.
   *
   *  @param x1 the value of x1
   *  @param x2 the value of x2
   */
  def d22(x1: Double, x2: Double) = -3 * x1 * pow(x2, 2)
  
  def xVector(x1: Double, x2: Double) = DenseMatrix(x1, x2)
  def fVector(x1: Double, x2: Double) = DenseMatrix(f1(x1, x2), f2(x1, x2))
  def jacobian(x1: Double, x2: Double) = 
    DenseMatrix((d11(x1, x2), d12(x1, x2)), (d21(x1, x2), d22(x1, x2)))

  /** TODO: Investigate why condition norm(f) < norm(fk) does not fail in the
     *  third iteration and implement line search.
	 */
  results.add(
	"ERROR: Condition norm(f) < norm(fk) should fail in the third iteration."
  )
  results.add("Please see the source code for details.\n")
  
  /** Returns the value of x for which the function value is zero, by using
   *  Newton's method of approximation.
   *
   *  @param x1k the seed value of x1, used for the first iteration
   *  @param x2k the seed value of x2, used for the first iteration
   */ 
  def newton(x1k: Double, x2k: Double): DenseMatrix[Double] = {
    def xk = xVector(x1k, x2k)
	def fk = fVector(x1k, x2k)
	def jk = jacobian(x1k, x2k)
	 
	val x = xk - inv(jk) * fk
	val f = fk + jk * (x - xk)
	 
	val v11 = utils.rounded(xk(0,0),5 )
	val v12 = utils.rounded(xk(1,0),5 )
	val v21 = utils.rounded(fk(0,0),5 )
	val v22 = utils.rounded(fk(1,0),5 )
	
	results.add(s"The results in this iteration are:")
	results.add(s"Values of xk and fk: ($v11, $v12) and ($v21, $v22)")
	if (norm(f(::, 0)) < norm(fk(::, 0))) results.add("norm(f) < norm(fk)\n")
	else  results.add("norm(f) >= norm(fk)\n")
	 
	if (utils.rounded(fk(0,0), 5) == 0.0 && utils.rounded(fk(1,0), 5) == 0.0) fk
	else newton(x(0,0), x(1,0))
  }
  
  // Running the algorithm  
  val seedValue = (1.0, 1.0)
  val approximation = newton(seedValue._1, seedValue._2)

  results.add(
    "ERROR: Condition norm(f) < norm(fk) should fail in the third iteration."
  )
  results.add(
    "Please see source code for details. The final results are not correct:\n"
  )
  
  val a1 = utils.rounded(approximation(0,0), 5)
  val a2 = utils.rounded(approximation(1,0), 5)

  results.add(s"Approximation: x = $a1, $a2\n")
  
  // Writing the buffered results
  results.all('-', 80) 
}
