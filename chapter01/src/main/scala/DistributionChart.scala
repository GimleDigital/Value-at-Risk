package valueatrisk.chapter01

import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.BarChart
import scalafx.scene.chart.CategoryAxis
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.XYChart

import scala.collection.JavaConversions
/** Builds a bar chart.
 *
 *  @param chartTitle the main title of the chart
 *  @param categoryAxis the name of the category axis
 *  @param numberAxis the name of the number axis
 */
class DistributionChart(chartTitle: String, categoryAxis: String,
                        numberAxis: String) {
  /** Returns a bar chart that can be displayed
   *
   *  @param categories the labels of the category axis
   *  @param values the value for each category
   */
  def getChart(categories: Array[Int], values: Seq[Number]) = {
    val categoriesBuffer = new ObservableBuffer[String]

    for (item <- categories) categoriesBuffer += item.toString

    val xAxis = CategoryAxis(categoriesBuffer)
    val yAxis = NumberAxis(numberAxis)

    def xyData(ys: Seq[Number]) = ObservableBuffer(
      categoriesBuffer zip ys map (xy => XYChart.Data(xy._1, xy._2)))

    val series1 = XYChart.Series(categoryAxis, xyData(values))

    new BarChart[String, Number](xAxis, yAxis) {
      title = chartTitle
      data = Seq(series1)
      categoryGap = 2.0
      barGap = 0.0
    }
  }
}