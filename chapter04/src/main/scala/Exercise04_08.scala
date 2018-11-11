package valueatrisk.chapter04

import scala.collection.mutable.ArrayBuffer

import java.io.File

import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.{TableCell, TableColumn, TableView}
import scalafx.scene.paint.Color._
import scalafx.scene.layout.GridPane
import scalafx.scene.text.Text

import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.stats.distributions.Binomial

/** Proposed solution for exercise 4.8 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Excercise04_08 extends JFXApp {
  // Exercise definitions
  val trials = 100
  val probabilities = Array(0.50, 0.48, 0.40)
  val intervals = Array((0.05, 40, 60), (0.01, 36, 62))

  val distributions = new ArrayBuffer[BinomialExtras]
  for (p <- probabilities) distributions.append(new BinomialExtras(trials, p))

  /** Represets a table row where the cells contains the probability of the
   *  distribution; the probabilities for the upper and lower bounds; and the
   *  power of the test, respectively.
   *
   *  @param dist the distribution
   *  @param lower the lower bound
   *  @param upper the upper bound
   */
  class DataRow(dist: BinomialExtras, lower: Int, upper: Int) {
    val probability = new StringProperty(this, "probability",
                          utils.doubleToString(dist.p, 2))
    val lowerP = new StringProperty(this, "lowerP",
                                    utils.doubleToString(dist.cdf(lower), 3))
    val upperP = new StringProperty(this, "upperP",
                                    utils.doubleToString(1.0 - dist.cdf(upper), 3))
    val power = new StringProperty(this, "power",
                                    utils.doubleToString(dist.cdf(lower) + 1.0 -
                                    dist.cdf(upper), 3))
  }

  // We need an array with one observable buffer per confidence interval
  val dataRows = new ArrayBuffer[ObservableBuffer[DataRow]]

  for (i <- 0 to intervals.length - 1) {
    dataRows.append(new ObservableBuffer[DataRow]())

    for (d <- 0 to distributions.length - 1)
      dataRows(i).append(
        new DataRow(distributions(d), intervals(i)._2, intervals(i)._3))
  }

  // Defining the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 4.8"
    height = 140 + (probabilities.length + 1) * 25

    // We create an array for each type of GUI element
    val headers = new ArrayBuffer[Label]
    val tableViews = new ArrayBuffer[TableView[DataRow]]

    // One set of GUI elements per confidence interval
    for (i <- 0 to intervals.length - 1) {
      headers.append(new Label {
        styleClass.add("exercise-4-8-label")
        text = s"${intervals(i)._1}% SIGNIFICANCE LEVEL TEST"
      })

      tableViews.append(new TableView[DataRow](dataRows(i)){
        styleClass.add("exercise-4-8-table-view")

        editable = false
        selectionModel = null
        columnResizePolicy = TableView.ConstrainedResizePolicy

        columns ++= List(
          new TableColumn[DataRow, String] {
            text = "p"
            cellValueFactory = { _.value.probability}
          },
          new TableColumn[DataRow, String] {
            text = s"Pr(S < ${intervals(i)._2 + 1})"
            cellValueFactory = { _.value.lowerP}
          },
          new TableColumn[DataRow, String] {
            text = s"Pr(S > ${intervals(i)._3})"
            cellValueFactory = { _.value.upperP}
          },
          new TableColumn[DataRow, String] {
            text = "Power"
            cellValueFactory = { _.value.power}
          }
        )
      })
    }

    // Displaying the graphical user interface
    scene = new Scene {
      stylesheets = List(getClass.getResource("styles.css").toExternalForm)

      root = new GridPane {
        styleClass.add("exercise-4-8-gridpane")

        for (i <- 0 to intervals.length - 1) {
          add(headers(i), 3 *  i, 0)
          add(tableViews(i), 3 * i, 1)
        }
      }
    }
  }
}
