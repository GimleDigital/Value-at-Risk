package valueatrisk.chapter14

import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.LineChart
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.XYChart

/** Provides a chart where a line and a scatter plot are combined, to be
 *  displayed in the GUI.
 *
 *  @param xText the text to be used as label for the X axis
 *  @param yText the text to be used as label for the Y axis
 */
class CombinedChart(xAxisLabel: String, yAxisLabel: String) {
  def getChart(line: Seq[(Int, Double)], plot: Seq[(Int, Double)]) = {
    val xAxis = NumberAxis(xAxisLabel)
    val yAxis = NumberAxis(yAxisLabel)

    val lineSeries = new XYChart.Series[Number, Number] {
      name = "VaR(t-1)"
      data = line.map((xy: (Int, Double)) => XYChart.Data[Number, Number](xy._1, xy._2))
    }
    val plotSeries = new XYChart.Series[Number, Number] {
      name = "P&L(t)"
      data = plot.map((xy: (Int, Double)) => XYChart.Data[Number, Number](xy._1, xy._2))
    }

    new LineChart[Number, Number](xAxis, yAxis, ObservableBuffer(lineSeries,
      plotSeries)) {
        styleClass.add("combined-chart")
    }
  }
}