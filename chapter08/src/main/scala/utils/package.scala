package valueatrisk.chapter08

import scala.math.{pow, round}
import scala.collection.mutable.ListBuffer

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

  def doubleToString(value: Double, decimals: Int): String = {
    val roundedValue = rounded(value, decimals)
    s"%.${decimals}f".formatLocal(java.util.Locale.US, roundedValue)
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
