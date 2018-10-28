package valueatrisk.chapter02

import scala.math.pow

import breeze.linalg.{DenseMatrix}

/** Proposed solution for exercise 2.25 in the book "Value-at-Risk: Theory and
 *  Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise02_25 extends App {
  // Results will be stored in a buffer, to be written later
  val results = new utils.Results("2.25")
  
  // Exercise definitions
  val x1Max = 2.0
  val x1Min = 1.0
  val x2Max = 5.0
  val x2Min = 3.0
    
  val partitions = 6
  
  def innerFunction = (x1: Double, x2: Double) => x1 * pow(x2, 3)

  /** Returns the approximation of an integral, obtained via a quadrature rule.
   *
   *  @param f the function to be evaluated
   *  @param m the number of partitions
   */  
  def quadratureIntegral(f: (Double, Double) => Double, m: Int): Double = {
	def multiplier(k: Int) = {
	  if (k == 0 || k == m) 1.0
	  else if (k % 2 == 1) 4.0
	  else 2.0
	}
	
	/** Returns the current accumulated value, after performing m iterations
	 *  over k2
	 *
	 *  @param k1 the remaining iterations over k1
	 *  @param k2 the remaining iterations over k2
	 *  @param prev the previously accumulated value
	 */	
    def quadrature2(k1: Int, k2: Int, prev: Double): Double = {
      val x1 = 1.0 + k2.toDouble / partitions.toDouble
	  val x2 = 3.0 + 2.0 * k1.toDouble / partitions.toDouble
		  
	  val w1 = (1.0 / partitions.toFloat) * multiplier(k2) / 3.0
	  val w2 = (2.0 / partitions.toFloat) * multiplier(k1) / 3.0
	  
	  val increment = (w1 * w2) * f(x1, x2)
	  
	  k2 match {
	    case 0 => prev + increment
	    case _ => {	    
		  quadrature2(k1, k2 -1, prev + increment) 
		}
	  }
	}
	
	/** Returns the current accumulated value, after performing m iterations
	 *  over k1
	 *
	 *  @param k1 the remaining iterations over k1
	 *  @param k2 the remaining iterations over k2
	 *  @param prev the previously accumulated value
	 */	
	def quadrature1(k1: Int, k2: Int, prev: Double): Double = {
	  k1 match {
	    case -1 => prev
	    case _ => {
		  val next = quadrature2(k1, k2, prev)
          quadrature1(k1 - 1, k2, next)
        }		  
	  }
	}
	
	quadrature1(m, m, 0)
  }
  
  // Analytical evaluation, outer Function: (3.0 / 2.0) * pow(x2, 3)
  def outerAntiderivative(x2: Double) = (3.0 / 8.0) * pow(x2, 4)
	
  val analyticalIntegral = outerAntiderivative(x2Max) - 
                           outerAntiderivative(x2Min)
  
  results.add(s"Analytical solution: = $analyticalIntegral\n")
  
  // Approximation with a quadrature rule
  val q = utils.rounded(quadratureIntegral(innerFunction, partitions), 1)
  
  results.add(s"Quadrature rule => $q\n")
  
  // Writing the buffered results
  results.all('-', 80) 
}
