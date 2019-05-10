package valueatrisk.chapter06

import utils._

import scala.collection.mutable.ArrayBuffer

import breeze.linalg._

/** Builds a collection of time series with prices calculated in different ways.
 *  - First nearby: the price available from the nearest future quote
 *  - Second nearby: the price available from the second nearest future quote
 *  - Price adjusted first and second nearby prices
 *  - Return adjusted first and second nearby prices
 *
 *  @param dataset a two-dimensional array of quoted prices
 */
class FuturesPrices(dataset: ArrayBuffer[Array[Double]]) {
  private val dates = (-1.0 +=: (dataset.map{_(0)}.distinct)).toArray
  private val tenors = (-1.0 +=: (dataset.map{_(1)}.distinct)).toArray
  
  /** Limit date when building the time series.TODO: Obtain these by analysing
   *  the dataset
   */
  private val begin = 199807.0
  private val end = 199906.0
  
  // Buffers which will contain the time series
  private val firstNearbys = new ArrayBuffer[String]
  private val secondNearbys = new ArrayBuffer[String]
  private val firstPriceAdjustments = new ArrayBuffer[String]
  private val secondPriceAdjustments = new ArrayBuffer[String]
  private val priceAdjustedFirstNearbys = new ArrayBuffer[String]
  private val priceAdjustedSecondNearbys = new ArrayBuffer[String]
  private val returnAdjustedFirstNearbys = new ArrayBuffer[String]
  private val returnAdjustedSecondNearbys = new ArrayBuffer[String]

  // Nearbys an adjustments are easier to build from data in matrix form
  private val dataMatrix = 
    DenseMatrix.tabulate(dates.length, tenors.length){case (i, j) => -1.0}

  /** Fills the matrix with dates in the first row and column; and successively
   *  with price quotes obtained from the dataset.
   */
  private def fillMatrix(): Unit = {
    dataMatrix(::, 0) := new DenseVector(dates)
    dataMatrix(0, ::) := new DenseVector(tenors).t
    for (item <- dataset) {
      for (n <- 1 to dataMatrix.rows - 1) {
        for (m <- 1 to dataMatrix.cols - 1) {
	      if (item(0) == dataMatrix(n, 0) && item(1) == dataMatrix(0, m))
	        dataMatrix(n, m) = item(2)
	    }
      }
	}
  }
  /** Recursively builds an array of first nearby prices
   *
   * @param lastDate the previous date for which the price was obtained
   * @param n the row number
   * @param m the column number
   */
  private def setFirstNearby(lastDate: Double, n: Int, m: Int): Unit = { 
    if (m < dataMatrix.cols) {
	  if (n < dataMatrix.rows) {
	    if (dataMatrix(n, 0) > lastDate && dataMatrix(n, m) != -1) {	
		  firstNearbys.append(s"${dataMatrix(n, 0)}	${dataMatrix(n, m)}")
		  setFirstNearby(dataMatrix(n, 0), n + 1, m)
        }
		else setFirstNearby(lastDate, n + 1, m)		  
	  }
	  else setFirstNearby(lastDate, 1, m + 1)
	}
  }

  /** Recursively builds an array of second nearby prices
   *
   * @param lastDate the previous date for which a price was obtained
   * @param n the row number
   * @param m the column number
   */
  private def setSecondNearby(lastDate: Double, n: Int, m: Int): Unit = {
    if (m + 1 < dataMatrix.cols) {
	  if (n < dataMatrix.rows) {
	    if (dataMatrix(n, 0) > lastDate && dataMatrix(n, m) != -1 && 
		    dataMatrix(n, m + 1) != -1) {
		  secondNearbys.append(
		    s"${dataMatrix(n, 0)}	${dataMatrix(n, m + 1)}")
		  setSecondNearby(dataMatrix(n, 0), n + 1, m)
		}
		else setSecondNearby(lastDate, n + 1, m)
	  }
	  else setSecondNearby(lastDate, 1, m + 1)
	}
  }

  /** Recursively builds an array of price adjustments for first nearbys
   *
   * @param n the current price matrix row number
   * @param m the current price matrix column number
   */  
  private def setFirstPriceAdjustments(n: Int, m: Int): Unit = {
    if (m + 1 < dataMatrix.cols) {
	  if (n + 1 < dataMatrix.rows) {
	    if (dataMatrix(n, m) != -1 && dataMatrix(n + 1, m) == -1 && 
		    dataMatrix(n, m + 1) != -1) {
		  firstPriceAdjustments.append(
		    s"${dataMatrix(n, 0)}	${rounded(dataMatrix(n, m + 1) - dataMatrix(n, m), 1)}")
		}
		setFirstPriceAdjustments(n + 1, m)
	  }
	  else setFirstPriceAdjustments(1, m + 1)
	}
  }
  
  /** Recursively builds an array of price adjustments for second nearbys.
   *
   * @param n the current price matrix row number
   * @param m the current price matrix column number
   */  
  private def setSecondPriceAdjustments(n: Int, m: Int): Unit = {
    if (m + 2 < dataMatrix.cols) {
	  if (n + 1 < dataMatrix.rows) {
	    if (dataMatrix(n, m) != -1 && dataMatrix(n + 1, m) == -1 &&
         	dataMatrix(n, m + 1) != -1) {
		  secondPriceAdjustments.append(
		    s"${dataMatrix(n, 0)}	${rounded(dataMatrix(n, m + 2) - dataMatrix(n, m + 1), 1)}")
		}
		setSecondPriceAdjustments(n + 1, m)
	  }
	  else setSecondPriceAdjustments(1, m + 1)
	}
  }
  
  // Recursively builds an array of price adjusted first nearbys.  
  private def setPriceAdjustedFirstNearbys (): Unit = {
	for (nearby <- firstNearbys) {
	  val date = nearby.split('	')(0).toDouble
	  val value = nearby.split('	')(1).toDouble
	  
	  if (date >= begin && date <= end) {
        var increment = 0.0

        for (adjustment <- firstPriceAdjustments) {
		  val rollover = adjustment.split('	')(0).toDouble
		  if (date <= rollover && begin <= rollover && rollover < end)
		    increment += adjustment.split('	')(1).toDouble
		}
		
        priceAdjustedFirstNearbys.append(
		  s"${date}	${rounded(value + increment, 1)}")
	  }
	}
  }

  // Recursively builds an array of price adjusted second nearbys.
  private def setPriceAdjustedSecondNearbys (): Unit = {
	for (nearby <- secondNearbys) {
	  val date = nearby.split('	')(0).toDouble
	  val value = nearby.split('	')(1).toDouble
	  
	  
	  if (date >= begin && date <= end) {
        var increment = 0.0

        for (adjustment <- secondPriceAdjustments) {
		  val rollover = adjustment.split('	')(0).toDouble
		  if (date <= rollover && begin <= rollover && rollover < end)
		    increment += adjustment.split('	')(1).toDouble
		}
       
        priceAdjustedSecondNearbys.append(
		  s"${date}	${rounded(value + increment, 1)}")
	  }
	}
  }

  /** Recursively builds an array of return adjusted first nearbys.
   *
   * @param n the current price matrix row number
   * @param m the current price matrix column number
   * @param lastValue the previous return adjusted first nearby
   */
  private def setReturnAdjustedFirstNearbys(n: Int, m: Int,
                                    lastValue: Double): Unit = {
    if (dataMatrix(n, 0) > end) {
	  setReturnAdjustedFirstNearbys (n - 1, dataMatrix.cols - 1, lastValue)
	}
	else {
	  if (dataMatrix(n, 0) >= begin) {
	    if (dataMatrix(n, m - 1) != -1.0 && m > 1) {
		  setReturnAdjustedFirstNearbys(n, m - 1, lastValue)
		}
		else {
		  val newValue = {
		    if (lastValue == 0 ) dataMatrix(n, m)
			else if (dataMatrix(n + 1, m) != -1) lastValue * (1 - 
			  (dataMatrix(n + 1, m) - dataMatrix(n, m)) / dataMatrix(n + 1, m))
			else lastValue * (1 - (dataMatrix(n + 1, m + 1) - 
			  dataMatrix(n, m + 1)) / dataMatrix(n + 1, m + 1))
		  }
	      returnAdjustedFirstNearbys.append(
		    s"${dataMatrix(n, 0)}	${rounded(newValue, 1)}")
		  setReturnAdjustedFirstNearbys(n - 1, m, newValue)
		}
	  }
	}
  }
    
  /** Recursively builds an array of return adjusted second nearbys.
   *
   * @param n the current price matrix row number
   * @param m the current price matrix column number
   * @param lastValue the previous return adjusted second nearby
   */
  private def setReturnAdjustedSecondNearbys(n: Int, m: Int, 
                                     lastValue: Double): Unit = {
    if (dataMatrix(n, 0) > end) {
	  setReturnAdjustedSecondNearbys (n - 1, dataMatrix.cols - 1, lastValue)
	}
	else {
	  if (dataMatrix(n, 0) >= begin) {
	    if (dataMatrix(n, m - 2) != -1.0 && dataMatrix(n + 1, m - 2) != -1.0 &&
            m > 2) {
		  setReturnAdjustedSecondNearbys(n, m - 1, lastValue)
		}
		else {
		  val newValue = {
		    if (lastValue == 0 ) dataMatrix(n, m)
			else if (dataMatrix(n + 1, m) != -1) {
			  lastValue * (1 - (dataMatrix(n + 1, m) - dataMatrix(n, m)) /
              dataMatrix(n + 1, m))
			}
			else lastValue * (1 - (dataMatrix(n + 1, m + 1) - 
			  dataMatrix(n, m + 1)) / dataMatrix(n + 1, m + 1))
		  }
	      returnAdjustedSecondNearbys.append(
		    s"${dataMatrix(n, 0)}	${rounded(newValue, 1)}")
		  setReturnAdjustedSecondNearbys(n - 1, m, newValue)
		}
	  }
	}
  }

  // Builds all arrays of prices and adjustments.  
  def setNearbys {
    fillMatrix
	
	setFirstNearby(0, 1, 1)
    setSecondNearby(0, 1, 1)
    setFirstPriceAdjustments(1, 1)
    setSecondPriceAdjustments(1, 1)
    setPriceAdjustedFirstNearbys()
    setPriceAdjustedSecondNearbys()
	setReturnAdjustedFirstNearbys(dataMatrix.rows - 1, dataMatrix.cols - 1, 0)
    setReturnAdjustedSecondNearbys(dataMatrix.rows - 1,
                                   dataMatrix.cols - 1, 0)
  }
   
  // Returns the array of first nearbys.
  def getFirstNearbys(): ArrayBuffer[String] = {
    firstNearbys
  }
  
  // Returns the array of second nearbys.
  def getSecondNearbys(): ArrayBuffer[String] = {
    secondNearbys
  }

  // Returns the array of price adjusted first nearby prices.
  def getPriceAdjustedFirstNearbys(): ArrayBuffer[String] = {
    priceAdjustedFirstNearbys
  }

  // Returns the array of price adjusted second nearby prices.
  def getPriceAdjustedSecondNearbys(): ArrayBuffer[String] = {
    priceAdjustedSecondNearbys
  }
  
  // Returns the array of return adjusted first nearby prices.
  def getReturnAdjustedFirstNearbys(): ArrayBuffer[String] = {
    returnAdjustedFirstNearbys.reverse
  }

  // Returns the array of return adjusted second nearby prices.
  def getReturnAdjustedSecondNearbys(): ArrayBuffer[String] = {
    returnAdjustedSecondNearbys.reverse
  }
}