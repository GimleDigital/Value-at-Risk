package valueatrisk.chapter05

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.{pow, sqrt}

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.mllib.linalg._
import org.apache.spark.mllib.stat.Statistics

/** Proposed solution for exercise 5.1 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise05_01 extends App {
 // Results are buffered and printed later
  val results = new Results("5.1")

  // Connecting to Apache Spark
  val conf: SparkConf =
    new SparkConf()
     .setMaster("local[*]")
     .setAppName("Exercise 5.1")
     .set("spark.driver.host", "localhost")

  val sc = new SparkContext(conf)

  sc.setLogLevel("WARN")

  // Loading the data
  val dataset = (sc.textFile("data/exercise-05-01-sample.dat")).map(_.toDouble)

  val buffer = new ArrayBuffer[Double]

  val iter = dataset.toLocalIterator

  while (iter.hasNext) {
    buffer += iter.next
  }

  // 1. Sample mean
  val mean = dataset.reduce(_ + _) / dataset.count

  results.add(s"Mean: ${rounded(mean, 3)}")

  // 2. Sample standard deviation

  val stdDev = sqrt(dataset.map(_ - mean).map(pow(_, 2)).reduce(_ + _) / dataset.count)

  results.add(s"Standard deviation: ${rounded(stdDev, 3)}")

  // Autocorrelation with lag = 1

  val lag01 = new ArrayBuffer[Double]
  val lag11 = new ArrayBuffer[Double]

  for (i <- 1 to buffer.length - 1) {
    lag01 += buffer(i)
    lag11 += buffer(i - 1)
  }
  val lagsRdd01 = sc.parallelize(lag01)
  val lagsRdd11 = sc.parallelize(lag11)

  val correlation1: Double = Statistics.corr(lagsRdd01, lagsRdd11, "pearson")

  results.add(s"Autocorrelation, lag = 1: ${rounded(correlation1, 3)}")

  // Autocorrelation with lag = 2

  val lag02 = new ArrayBuffer[Double]
  val lag22 = new ArrayBuffer[Double]

  for (i <- 2 to buffer.length - 1) {
    lag02 += buffer(i)
    lag22 += buffer(i - 2)
  }

  val lagsRdd02= sc.parallelize(lag02)
  val lagsRdd22= sc.parallelize(lag22)

  val correlation2: Double = Statistics.corr(lagsRdd02, lagsRdd22, "pearson")

  results.add(s"Autocorrelation, lag = 2: ${rounded(correlation2, 3)}")

  // Autocorrelation with lag = 3

  val lag03 = new ArrayBuffer[Double]
  val lag33 = new ArrayBuffer[Double]

  for (i <- 3 to buffer.length - 1) {
    lag03 += buffer(i)
    lag33 += buffer(i - 3)
  }

  val lagsRdd03 = sc.parallelize(lag03)
  val lagsRdd33 = sc.parallelize(lag33)

  val correlation3: Double = Statistics.corr(lagsRdd03, lagsRdd33, "pearson")

  results.add(s"Autocorrelation, lag = 3: ${rounded(correlation3, 3)}")

  // Printing the results
  results.all('-', 80)
}
