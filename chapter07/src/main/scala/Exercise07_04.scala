package valueatrisk.chapter07

import utils._

import scala.collection.mutable.ArrayBuffer

import java.io._

import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.{TableCell, TableColumn, TableView}
import scalafx.scene.layout.{VBox}

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Gaussian

/** Proposed solution for exercise 7.4 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise07_04 extends JFXApp {

  // Exercise definitions
  val gaussian1 = Gaussian(0, 25)
  val gaussian2 = Gaussian(0, 9)
  val gaussian3 = Gaussian(0, 1)

  val sample1 = DenseMatrix.rand(500, 1, gaussian1)
  val sample2 = DenseMatrix.rand(500, 1, gaussian2)
  val sample3 = DenseMatrix.rand(500, 1, gaussian3)

  val sampleOwn =
    DenseMatrix.vertcat(sample1, DenseMatrix.vertcat(sample2, sample3))

  val sampleAuthor = csvread(new File("data/exercise-07-04-sample.dat"), '	')

  /** Choose author's or own sample by commenting one of these definitions. */
  // val sample = sampleOwn
  val sample = sampleAuthor

  // Preparing time series for graph display
  val observations = new ArrayBuffer[(Int, Double)]

  for (i <- 0 to sample.rows - 1) {
    observations.append((i + 1, sample(i, 0)))
  }

  // Preparing statistical measures for table display
  val mean1 = sample(0 to 499, 0).sum / 500.0
  val mean2 = sample(500 to 999, 0).sum / 500.0
  val mean3 = sample(1000 to 1499, 0).sum / 500.0
  val meanA = sample.sum / 1500.0

  val stdDev1 = sqrt(sample(0 to 499, 0).map(_ - mean1).map(pow(_, 2)).sum /
    500)
  val stdDev2 = sqrt(sample(500 to 999, 0).map(_ - mean2).map(pow(_, 2)).sum /
    500)
  val stdDev3 = sqrt(sample(1000 to 1499, 0).map(_ - mean3).map(pow(_, 2)).sum /
    500)
  val stdDevA = sqrt(sample.map(_ - meanA).map(pow(_, 2)).sum / 1500)

  val skewness1 = sample(0 to 499, 0).map(_ - mean1).map(pow(_, 3)).sum /
    (500 * pow(stdDev1, 3))
  val skewness2 = sample(500 to 999, 0).map(_ - mean2).map(pow(_, 3)).sum /
    (500 * pow(stdDev2, 3))
  val skewness3 = sample(1000 to 1499, 0).map(_ - mean3).map(pow(_, 3)).sum /
    (500 * pow(stdDev3, 3))
  val skewnessA = sample.map(_ - meanA).map(pow(_, 3)).sum /
    (1500 * pow(stdDevA, 3))

  val kurtosis1 = sample(0 to 499, 0).map(_ - mean1).map(pow(_, 4)).sum /
    (500 * pow(stdDev1, 4))
  val kurtosis2 = sample(500 to 999, 0).map(_ - mean2).map(pow(_, 4)).sum /
    (500 * pow(stdDev2, 4))
  val kurtosis3 = sample(1000 to 1499, 0).map(_ - mean3).map(pow(_, 4)).sum /
    (500 * pow(stdDev3, 4))
  val kurtosisA = sample.map(_ - meanA).map(pow(_, 4)).sum /
    (1500 * pow(stdDevA, 4))

  // Building a collection of rows, to be used in a table view.

  case class DataRow(label: String, first500: String, second500: String,
                     third500: String, all1500: String)

  val dataRows = ObservableBuffer[DataRow]()

  dataRows += new DataRow(
    "Mean", doubleToString(mean1, 2), doubleToString(mean2, 2),
            doubleToString(mean3, 2), doubleToString(meanA, 2))
  dataRows += new DataRow(
    "Standard deviation", doubleToString(stdDev1, 2), doubleToString(stdDev2, 2),
                          doubleToString(stdDev3, 2), doubleToString(stdDevA, 2))
  dataRows += new DataRow(
    "Skewness", doubleToString(skewness1, 2), doubleToString(skewness2, 2),
                doubleToString(skewness3, 2), doubleToString(skewnessA, 2))
  dataRows += new DataRow(
    "Kurtosis", doubleToString(kurtosis1, 2), doubleToString(kurtosis2, 2),
                doubleToString(kurtosis3, 2), doubleToString(kurtosisA, 2))

  // Defining the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 7.4"

    width = 1000
    height = 720

    val header1 = new Label {
      styleClass.add("exercise-7-4-label")

      text = "SAMPLE TIME SERIES"
    }

    val timeSeriesChart = new TimeSeriesChart("Time", "Values")

    val header2 = new Label {
      styleClass.add("exercise-7-4-label")

      text = "COMPARISION OF STATISTICS"
    }

    val tableView = new TableView(dataRows) {
      styleClass.add("exercise-7-4")

      editable = false
      selectionModel = null
      columnResizePolicy = TableView.ConstrainedResizePolicy
    }

    val col1 = new TableColumn[DataRow, String]("") {
      styleClass.add("first-column")
    }
    col1.cellValueFactory = cdf => StringProperty(cdf.value.label)

    val col2 = new TableColumn[DataRow, String]("First 500")
    col2.cellValueFactory = cdf => StringProperty(cdf.value.first500)

    val col3 = new TableColumn[DataRow, String]("Second 500")
    col3.cellValueFactory = cdf => StringProperty(cdf.value.second500)

    val col4 = new TableColumn[DataRow, String]("Third 500")
    col4.cellValueFactory = cdf => StringProperty(cdf.value.third500)

    val col5 = new TableColumn[DataRow, String]("All 1500")
    col5.cellValueFactory = cdf => StringProperty(cdf.value.all1500)

    tableView.columns ++= List(col1, col2, col3, col4, col5)

    // Displaying the graphical user interface
    scene = new Scene {
      stylesheets = List(getClass.getResource("styles.css").toExternalForm)

      root = new VBox {
        padding = Insets(15, 15, 15, 15)
      }

      content.addAll(header1, timeSeriesChart.getContent(observations.toSeq),
                     header2, tableView)
    }
  }
}
