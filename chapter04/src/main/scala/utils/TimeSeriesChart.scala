package valueatrisk.chapter04

import scala.math.{max, min}

import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.LineChart
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.XYChart

/** Represents a time series chart, to be displayed in the GUI.
 *
 *  @param xText the text to be used as label for the X axis
 *  @param yText the text to be used as label for the Y axis
 */
class TimeSeriesChart(xText: String, yText: String) {
  /** Returns the complete time series chart.
   *
   *  @param dataSet the data points to be used in the chart
   */
  def getContent(dataSet: Seq[(Int, Double)]) = {
    val t = dataSet.size
    val xAxis = NumberAxis(xText, 0, t, t)
    val yAxis = NumberAxis(yText)
    val timeSeries = new XYChart.Series[Number, Number] {
      name = "Time Series"
      data = dataSet.map((xy: (Int, Double)) => XYChart.Data[Number, Number](xy._1, xy._2))
    }

    new LineChart[Number, Number](xAxis, yAxis, ObservableBuffer(timeSeries))
  }
}
