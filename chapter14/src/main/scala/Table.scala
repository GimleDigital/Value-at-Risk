package valueatrisk.chapter14

import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{TableCell, TableColumn, TableView}

/** Provides utilities for creating a ScalaFX table and fill it with data.
 *
 * @param header the column headers
 * @param dataset the dataset to be used written in the table
 * @param style the css style class (optional)
 */
class Table(header: Array[String], dataset: Array[Array[String]]) {
  /** Represents a row of data in the table.
   *
   *  @param args the data elemenst (number of elements resolved dynamically)
   */
  class DataRow(args: String*) {
    def getArg(i: Int) = args(i)
  }

  val dataRows = ObservableBuffer[DataRow]()

  val tableView = new TableView(dataRows) {
      styleClass.add("exercise-10-6")

    editable = false
    selectionModel = null
    columnResizePolicy = TableView.ConstrainedResizePolicy
  }

  for (data <- dataset) dataRows += new DataRow(data:_*)

  // Builds and returns the table
  def getTable() = {
    for (i <- 0 to header.length - 1) {
      val col = new TableColumn[DataRow, String](header(i))

      col.cellValueFactory = cdf => StringProperty(cdf.value.getArg(i))

      tableView.columns += col
    }

    tableView
  }
}