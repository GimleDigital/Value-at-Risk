package valueatrisk.chapter04

import scala.collection.mutable.ArrayBuffer

import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.{TableCell, TableColumn, TableView}
import scalafx.scene.paint.Color._
import scalafx.scene.layout.{GridPane}
import scalafx.scene.text.Text

import java.io.File

import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants._

/** Proposed solution for exercise 4.7 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise04_07 extends JFXApp {
  // Exercise definitions
  val trials = 100
  val probability = 0.50

  val binom = new BinomialExtras(trials, probability)

  val lowerBounds = (36 to 40).toArray
  val upperBounds = (59 to 67).toArray

  /** Represets a table row, where the first cell contains the upper bound label
   *  and the other cells contain the probability values that correspond to the
   *  current upper bound and the lower bound for each column.
   *
   *  @param i the index of the item to use in the array of upper bounds.
   */
  class DataRow(i: Int) {
    val obs = new ObservableBuffer[StringProperty]

    obs += new StringProperty(this, upperBounds(i).toString,
                              upperBounds(i).toString)

    for (j <- 0 to lowerBounds.length - 1) {
      val prob = 1.0 - binom.cdf(upperBounds(i)) + binom.cdf(lowerBounds(j) - 1)

      obs += new StringProperty(this, utils.doubleToString(prob, 6),
                                utils.doubleToString(prob, 6))
    }
  }

  // Building a collection of rows, to be used in a table view.
  val dataRows = ObservableBuffer[DataRow]()

  for (i <- 0 to upperBounds.length - 1) {
    dataRows += new DataRow(i)
  }

  // Defining the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 4.7"
    height = 425

    val header = new Label {
      styleClass.add("exercise-4-7-label")

      text = "PROBABILITIES OF TYPE 1 ERROR"
    }

    val yLabel = new Label {
      styleClass.add("exercise-4-7-label")

      text = "Interval Upper Bound"
      margin = Insets(-50)
      rotate = -90
      wrapText = true
    }
    val xLabel = new Label {
      styleClass.add("exercise-4-7-label")

      text = "Interval Lower Bound"
      padding = Insets(10)
    }

    /** The table view contains a first column with an empty header and one
     *  column for each lower bound
     */
    val tableview = new TableView[DataRow](dataRows) {
      styleClass.add("exercise-4-7")

      editable = false
      selectionModel = null
      columnResizePolicy = TableView.ConstrainedResizePolicy

      columns += new TableColumn[DataRow, String] {
          cellValueFactory = { _.value.obs(0) }
          styleClass.add("first-column")
      }

      for(i <- 1 to lowerBounds.length) {
        columns += new TableColumn[DataRow, String] {
          text = lowerBounds(i - 1).toString
          cellValueFactory = { _.value.obs(i) }
        }
      }
    }

    // Displaying the graphical user interface
    scene = new Scene {
      stylesheets = List(getClass.getResource("styles.css").toExternalForm)

      root = new GridPane {
        padding = Insets(10, 60, 50, 0)
        add(header, 1, 0)
        add(yLabel, 0, 2)
        add(xLabel, 1, 1)
        add(tableview, 1, 2)
      }
    }
  }
}
