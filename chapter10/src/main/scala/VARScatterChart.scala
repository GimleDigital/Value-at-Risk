package valueatrisk.chapter10

import javafx.scene.{chart => jfxsc}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{NumberAxis, ScatterChart, XYChart}
import scalafx.scene.layout.StackPane

class VARScatterChart(xAxis: NumberAxis, yAxis: NumberAxis) {
  private val chart = new ScatterChart(xAxis, yAxis)

  def getChart = chart

  def setChartData(xData: Array[Double], yData: Array[Double]): ObservableBuffer[jfxsc.XYChart.Series[Number, Number]] = {
    val data = new ObservableBuffer[jfxsc.XYChart.Series[Number, Number]]()
    val chart = new XYChart.Series[Number, Number] {
      name = "pt(r), pq(r)"
    }
    
    for (i <- 0 to xData.length - 1) 
      chart.data() += XYChart.Data[Number, Number](xData(i), yData(i))
 
    data.addAll(chart)
	
    data
}

}