package valueatrisk.chapter14

import javafx.scene.{chart => jfxsc}

import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.LineChart
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.XYChart

class VARScatterChart(xAxisLabel: String, yAxisLabel: String) {
  def getChart(plot: Seq[(Double, Double)]) = {
    val xAxis = NumberAxis(xAxisLabel)
    val yAxis = NumberAxis(yAxisLabel)

    val plotSeries = new XYChart.Series[Number, Number] {
      name = "n(j), n*(j)"
      data = plot.map((xy: (Double, Double)) => XYChart.Data[Number, Number](xy._1, xy._2))
    }

    new LineChart[Number, Number](xAxis, yAxis, ObservableBuffer(plotSeries)) {
        styleClass.add("var-scatter-chart")
    }
  }
}