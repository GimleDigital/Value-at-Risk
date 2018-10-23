package valueatrisk

import scala.collection.mutable.ListBuffer

/** Utilities and help functions */
package object utils {
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
    def addResult(str: String) {
       listBuffer += str
    }

    /** Prints the results to console.
     *
     *  @param separator the character to be used as separator
     */
    def printResults(separator: Char) {
      val sep = separator.toString
      println(s"\n${sep*80}\n\nRESULTS FOR EXERCISE $exercise\n")

      for (item <- listBuffer) {
        println(s"$item")
      }

      println(s"${sep*80}\n")
    }
  }
}