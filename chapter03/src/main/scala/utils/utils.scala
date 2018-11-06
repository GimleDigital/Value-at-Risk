package valueatrisk.chapter03

import scala.collection.mutable.ListBuffer
import scala.math.{abs, pow, round, signum}

import breeze.linalg._

/** Utilities and help functions */
package object utils {
  /** Returns a rounded value.
   *
   *  @param number the number to be rounded
   *  @param precision the number of decimals
   */
  def rounded(number: Double, precision: Int): Double = {
    (round(number * pow(10, precision.toFloat)) /
    pow(10, precision.toFloat)).toDouble
  }

  /** Returns the sign (+ or -) of a number.
   *
   *  @param number the number to be examined
   */
  def getSign(number: Double) = if (signum(number) == 1) "+" else "-"

  /** Returns a formatted definition of a polynom with an arbitrary number of
   *  terms.
   *
   *  @param parameterValues the values of the equation's parameters
   *  @param functionName the name of the function, i.e,. the left hand side
   *  @param variableName the name of the variable in the equation
   */
  def getPolynomial(parameterValues: Array[Double], functionName: String,
                  variableName: String, rounding: Int): String = {
    /** Recursive function that matches an equation's parameters and variables.
     *
     *  @param params the array of parameters
     *  @param equation the equation's definition so far
     */
    def getItem(params: Array[Double], polynomial: String): String = {
      val absValue = abs(rounded(params.head, rounding))
      val sign =
            if (getSign(params.head) == "+" &&
                params.length == parameterValues.length) ""
            else s" ${getSign(rounded(params.head, rounding))}"

      params.length match {
        case 1 => if (absValue == 0) "" else s"$polynomial$sign $absValue"
        case 2 => {
          if (absValue == 0) getItem(params.tail, s"$polynomial")
          else getItem(params.tail, s"$polynomial$sign $absValue$variableName")
        }
        case _ => {
          if (absValue == 0) getItem(params.tail, s"$polynomial")
          else getItem(params.tail,
            s"$polynomial$sign $absValue$variableName${params.length-1}")
        }
      }
    }

    getItem(parameterValues, s"$functionName =")
  }

  /** Provides functions to accumulate results in a buffer and write them all
   *  together in the console. Ensures a clean output, separated from Breeze and
   *  Spark error messages,
   *
   *  @param exercise the exercise to which the results correspond
   */
  class Results(exercise: String) {
    // Results buffer
    val listBuffer = new ListBuffer[String]

    /** Adds a result to the results buffer.
     *
     *  @param str the string to be added
     */
    def add(str: String) {
       listBuffer += str
    }

    /** Prints the results to console, with lines of characters equal to console
     *  width before and after the block of results.
     *
     *  @param separator the character to be used as separator in the lines
     */
    def all(separator: Char, width: Int) = {
      val sep = separator.toString

      println(s"\n${sep*width}\n\nRESULTS FOR EXERCISE $exercise\n")

      for (item <- listBuffer) {
        println(s"$item")
      }

      println(s"${sep*width}\n")
    }
  }
}
