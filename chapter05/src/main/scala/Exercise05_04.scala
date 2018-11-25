package valueatrisk.chapter05

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.{exp, pow, sqrt}

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.mllib.linalg._
import org.apache.spark.mllib.random._
import org.apache.spark.mllib.stat.Statistics

/** Proposed solution for exercise 5.4 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise05_04 extends App {
  // Results are buffered and printed later
  val results = new Results("5.4")

  /** Returns the value of the changed value integral
   *
   *  @param u the values u for which the integral is evaluated
   */
   def integral(u: Array[Double]) = 2.0 * (2.0 * u(0) - 1.0) * pow(u(1), 2) / (u(2) + 1.0)

  /** Returns datasets with random values for sample sizes 1000, 100, 10 and 1.
   *
   *  @param ctx a valid Spark context
   */
  def randomValues(context: SparkContext) = {
    val uniformGenerator = new UniformGenerator
    val samples = new ArrayBuffer[Double]

    for (i <- 0 to 999) {
      samples += uniformGenerator.nextValue
    }

    samples
  }

  // Connecting to Apache Spark
  val conf: SparkConf =
    new SparkConf()
     .setMaster("local[*]")
     .setAppName("Exercise 5.4")
     .set("spark.driver.host", "localhost")

  val sc = new SparkContext(conf)

  sc.setLogLevel("WARN")

  // Generating the random sample datasets and obtaining the the value of the integral
  val rv1 = randomValues(sc)
  val rv2 = randomValues(sc)
  val rv3 = randomValues(sc)

  val sample1000 = new ArrayBuffer[Double]
  val sample100 = new ArrayBuffer[Double]
  val sample10 = new ArrayBuffer[Double]
  val sample1 = new ArrayBuffer[Double]

  for (i <- 0 to 999) sample1000 += integral(Array(rv1(i), rv2(i), rv3(i)))
  for (i <- 0 to 99) sample100 += sample1000(i)
  for (i <- 0 to 9) sample10 += sample100(i)
  sample1 += sample10(0)

  val sampleRDD1000 = sc.parallelize(sample1000)
  val sampleRDD100 = sc.parallelize(sample100)
  val sampleRDD10 = sc.parallelize(sample10)
  val sampleRDD1 = sc.parallelize(sample1)

  // Calculating the sample means
  val mean1 = sampleRDD1.reduce(_ + _) / sampleRDD1.count
  val mean10 = sampleRDD10.reduce(_ + _) / sampleRDD10.count
  val mean100 = sampleRDD100.reduce(_ + _) / sampleRDD100.count
  val mean1000 = sampleRDD1000.reduce(_ + _) / sampleRDD1000.count

  results.add(s"Mean (m = 1)    = ${rounded(mean1, 5)}")
  results.add(s"Mean (m = 10)   = ${rounded(mean10, 5)}")
  results.add(s"Mean (m = 100)  = ${rounded(mean100, 5)}")
  results.add(s"Mean (m = 1000) = ${rounded(mean1000, 5)}\n")

  // Calculating the sample's standard deviations
  val stdDev1 = sqrt(sampleRDD1000.map(_ - mean1000).map(pow(_, 2))
    .reduce(_ + _) / sampleRDD1000.count)
  val stdDev10 = stdDev1 / sqrt(10)
  val stdDev100 = stdDev1 / sqrt(100)
  val stdDev1000 = stdDev1 / sqrt(1000)

  results.add(s"Standard deviation (m = 1):    ${rounded(stdDev1, 5)}")
  results.add(s"Standard deviation (m = 10):   ${rounded(stdDev10, 5)}")
  results.add(s"Standard deviation (m = 100):  ${rounded(stdDev100, 5)}")
  results.add(s"Standard deviation (m = 1000): ${rounded(stdDev1000, 5)}\n")

  // Printing the results
  results.all('-', 80)
}
