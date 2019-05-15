package valueatrisk.chapter06

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import breeze.linalg._

/** Proposed solution for exercise 6.1 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise06_01 extends App {
  // Results are buffered and printed later
  val results = new Results("6.1")

  // Uploading exercise definitions to a dataset
  val fileName = "data/exercise-06-01-sample.dat"

  val dataset = new ArrayBuffer[Array[Double]]

  val fileBuffer = Source.fromFile(fileName).getLines

  for (item <- fileBuffer) {
    dataset.append(item.split('	').map(_.toDouble))
  }

  // Building the class that will contain prices and adjustments
  val futuresPrices = new FuturesPrices(dataset)
  futuresPrices.setNearbys

  // Obtaining the adjusted time series
  val priceAdjustedFirstNearbys = futuresPrices.getPriceAdjustedFirstNearbys()
  val priceAdjustedSecondNearbys = futuresPrices.getPriceAdjustedSecondNearbys()
  val returnAdjustedFirstNearbys = futuresPrices.getReturnAdjustedFirstNearbys()
  val returnAdjustedSecondNearbys = futuresPrices.getReturnAdjustedSecondNearbys()

  // Preparing the formatted results
  val priceAdjustedNearbys = new ArrayBuffer[String]
  val returnAdjustedNearbys = new ArrayBuffer[String]

  for (i <- 0 to priceAdjustedFirstNearbys.length - 1) {
    val date = priceAdjustedFirstNearbys(i).split('	')(0)
    val firstNearby = priceAdjustedFirstNearbys(i).split('	')(1)
    val secondNearby = priceAdjustedSecondNearbys(i).split('	')(1)

    priceAdjustedNearbys.append(s"${numberToLabel(date)}	$firstNearby	$secondNearby")
  }

  for (i <- 0 to returnAdjustedFirstNearbys.length - 1) {
    val date = returnAdjustedFirstNearbys(i).split('	')(0)
    val firstNearby = returnAdjustedFirstNearbys(i).split('	')(1)
    val secondNearby = returnAdjustedSecondNearbys(i).split('	')(1)

    returnAdjustedNearbys.append(s"${numberToLabel(date)}	$firstNearby	$secondNearby")
  }

  results.add("PRICE ADJUSTED NEARBYS")
  for (item <- priceAdjustedNearbys) results.add(item)

  results.add("RETURN ADJUSTED NEARBYS")
  for (item <- returnAdjustedNearbys) results.add(item)

  // Printing results
  results.all('-', 80)

  Thread.sleep(1000)
}
