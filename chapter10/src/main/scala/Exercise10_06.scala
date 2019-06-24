package valueatrisk.chapter10

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.{pow, sqrt}

import java.io._

import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.chart.{NumberAxis, ScatterChart}
import scalafx.scene.control.Label
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.{TableCell, TableColumn, TableView}
import scalafx.scene.layout.{GridPane, VBox}

import breeze.linalg._
import breeze.stats._

/** Proposed solution for exercise 10.6 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 *
 */
object Exercise10_06 extends JFXApp {
  // Exercise definitions
  val u = DenseMatrix(1.2, 1.6)
  val sigma = DenseMatrix((0.31, 0.09), (0.09, 0.22))
  val w = DenseMatrix(800.0, -300.0, -100.0, 250.0).t

  // a) Primary mapping
  def s(u: DenseMatrix[Double]) = DenseMatrix(100.0 * u(0, 0),
    100 * max(0.0, u(0, 0) - 1.25), 100 * max(0.0, u(1, 0) - 1.00),
    100 * max(0.0, u(1, 0) -1.50))

  def p(r: DenseMatrix[Double]) = w * s(r)

  // b) Realizations
  val r = csvread(new File("data/exercise-10-06-sample.dat"), ';')
  val pt = DenseMatrix.zeros[Double](r.cols, 1)

  val portfolioValues = new ArrayBuffer[Array[String]]

  for (k <- 0 to r.cols - 1) {
    pt(k, 0) = p(DenseMatrix(r(::, k)).t)(0, 0)

    portfolioValues.append(p(DenseMatrix(r(::, k)).t).map(doubleToString(_, 0))
      .toArray)
  }

  val headerB = Array("Portfolio values")
  val tableB = new Table(headerB, portfolioValues.toArray)

  // c) Quadratic remapping
  val qr = new QuadraticRemapping(r, pt)

  val f = qr.getF

  val beta = inv(f.t * f) * f.t * pt

  val parameterValues = new ArrayBuffer[Array[String]]

  val params = Array("c11", "c22", "2 * c12", "b1", "b2", "a")

  for (i <- 0 to params.length - 1) parameterValues.append(Array(params(i),
    doubleToString(beta(i, 0), 0)))

  val headerC = Array("Parameter", "Value")
  val tableC = new Table(headerC, parameterValues.toArray)

  // d) Scatter plot
  val cDot = DenseMatrix((beta(0, 0), beta(2, 0) / 2.0),
                       (beta(2, 0) / 2.0, beta(1, 0)))
  val bDot = DenseMatrix(beta(3, 0), beta(4, 0)).t
  val aDot = DenseMatrix(beta(5, 0))

  def pq(x: DenseMatrix[Double]) = x.t * cDot * x + bDot * x + aDot

  val sizeD = 100 // Size of pseudo-random sample

  val estimationD = new Estimation(sizeD, u, sigma)

  val pValues = estimationD.getValues(p)

  val pqValues = estimationD.getValues(pq)

  val xAxis = new NumberAxis() {
    styleClass.add("exercise-10-6-axis-label")

    label = "p(r)"
  }
  val yAxis = new NumberAxis() {
    styleClass.add("exercise-10-6-axis-label")

    label = "pq(r)"
  }

  val vsc = new VARScatterChart(xAxis, yAxis)

  val chartD = vsc.getChart

  chartD.data = vsc.setChartData(pValues, pqValues)

  // e) Crude Monte Carlo
  val sizeE = 1000 // Size of pseudo-random sample

  val estimationE = new Estimation(sizeE, u, sigma)

  val hE = estimationE.crudeMonteCarlo(p)

  // f) Control variate
  val stdPF = 38150.0

  val hF = estimationE.controlVariate(p, pq, stdPF)

  // g) Stratified Monte Carlo
  val ws = 16

  val cdf = csvread(new File("data/exercise-10-06-cdfe.dat"), ';')

  val hG = estimationE.stratifiedMonteCarlo(pq, ws, cdf)

  val parameterValuesEFG = new ArrayBuffer[Array[String]]
  val methods = Array("Crude Monte Carlo", "Control variate MC",
    "Stratified MC")

  parameterValuesEFG.append(Array(methods(0), doubleToString(hE, 0)))
  parameterValuesEFG.append(Array(methods(1), doubleToString(hF, 0)))
  parameterValuesEFG.append(Array(methods(2), doubleToString(hG, 0)))

  val headerEFG = Array("Method", "Std(P)")
  val tableEFG = new Table(headerEFG, parameterValuesEFG.toArray)

  // h) Standard errors
  val sizeH = 1000

  val crudeMonteCarlo = new ArrayBuffer[Double]
  val controlVariate = new ArrayBuffer[Double]
  val stratifiedMonteCarlo = new ArrayBuffer[Double]

  for (k <- 1 to 10) {
    val estimation = new Estimation(sizeH, u, sigma)

    crudeMonteCarlo.append(estimation.crudeMonteCarlo(p))
    controlVariate.append(estimation.controlVariate(p, pq, stdPF))
    stratifiedMonteCarlo.append(estimation.stratifiedMonteCarlo(p, ws, cdf))
  }

  val meansH = Array(crudeMonteCarlo.sum / crudeMonteCarlo.length,
                     controlVariate.sum / controlVariate.length,
                     stratifiedMonteCarlo.sum / stratifiedMonteCarlo.length)

  val stdDevsH = Array(
    sqrt(crudeMonteCarlo.map(_ - meansH(0)).map(pow(_, 2)).sum /
    (crudeMonteCarlo.length - 1)),
    sqrt(controlVariate.map(_ - meansH(1)).map(pow(_, 2)).sum /
    (controlVariate.length - 1)),
    sqrt(stratifiedMonteCarlo.map(_ - meansH(2)).map(pow(_, 2)).sum /
    (stratifiedMonteCarlo.length - 1)))

  val stdErrsH = Array(stdDevsH(0) / meansH(0), stdDevsH(1) / meansH(1),
    stdDevsH(2) / meansH(2))

  val parameterValuesH = new ArrayBuffer[Array[String]](3)

  parameterValuesH.append(Array(methods(0), doubleToString(meansH(0), 0),
    doubleToString(stdDevsH(0), 0),
    s"${doubleToString(stdErrsH(0) * 100.0, 2)}%"))
  parameterValuesH.append(Array(methods(1), doubleToString(meansH(1), 0),
    doubleToString(stdDevsH(1), 0),
    s"${doubleToString(stdErrsH(1) * 100.0, 2)}%"))
  parameterValuesH.append(Array(methods(2), doubleToString(meansH(2), 0),
    doubleToString(stdDevsH(2), 0),
    s"${doubleToString(stdErrsH(2) * 100.0, 2)}%"))

  val headerH = Array("Method", "Mean", "Std Dev", "Std Err")

  val tableH = new Table(headerH, parameterValuesH.toArray)

  // i) 1% standard error
  val size1 = sizeE
  val stdErr2 = 0.01 // Desired value

  def size2(stdErr1: Double) = pow(stdErr1 / stdErr2, 2) * size1.toDouble

  val sizes2 = Array(size2(stdErrsH(0)), size2(stdErrsH(1)), size2(stdErrsH(2)))

  val parameterValuesI = new ArrayBuffer[Array[String]](3)

  parameterValuesI.append(Array(methods(0), doubleToString(sizes2(0), 0)))
  parameterValuesI.append(Array(methods(1), doubleToString(sizes2(1), 0)))
  parameterValuesI.append(Array(methods(2), doubleToString(sizes2(2), 0)))

  val headerI = Array("Method", "Sample size")

  val tableI = new Table(headerI, parameterValuesI.toArray)

  // j) Crude Monte Carlo VaR
  val confidenceLevel = 0.95
  val portfolioValue = 89700.0

  val varJ = estimationE.crudeMonteCarloVaR(p, portfolioValue, confidenceLevel)

  // k) Control variate VaR
  val stdPK = 21770.0

  val varK = estimationE.controlVariateVaR(p, pq, stdPK, portfolioValue,
    confidenceLevel)

  // l) Stratified Monte Carlo VaR
  val varL = estimationE.stratifiedMonteCarloVaR(p, stdPK, portfolioValue,
    confidenceLevel)

  val parameterValuesJKL = new ArrayBuffer[Array[String]]

  parameterValuesJKL.append(Array(methods(0), doubleToString(varJ, 0)))
  parameterValuesJKL.append(Array(methods(1), doubleToString(varK, 0)))
  parameterValuesJKL.append(Array(methods(2), doubleToString(varL, 0)))

  val headerJKL = Array("Method", "VaR")
  val tableJKL = new Table(headerJKL, parameterValuesJKL.toArray)


  // m) VaR standard errors
  val sizeM = 1000

  val crudeMonteCarloVaR = new ArrayBuffer[Double]
  val controlVariateVaR = new ArrayBuffer[Double]
  val stratifiedMonteCarloVaR = new ArrayBuffer[Double]

  for (k <- 1 to 10) {
    val estimation = new Estimation(sizeM, u, sigma)

    crudeMonteCarloVaR.append(estimation.crudeMonteCarloVaR(p, portfolioValue,
      confidenceLevel))
    controlVariateVaR.append(estimation.controlVariateVaR(p, pq, stdPK,
     portfolioValue, confidenceLevel))
    stratifiedMonteCarloVaR.append(estimation.stratifiedMonteCarloVaR(pq,
      stdPK, portfolioValue, confidenceLevel))
  }

  val meansM = Array(crudeMonteCarloVaR.sum / crudeMonteCarloVaR.length,
    controlVariateVaR.sum / controlVariateVaR.length,
    stratifiedMonteCarloVaR.sum / stratifiedMonteCarloVaR.length)

  val stdDevsM = Array(
    sqrt(crudeMonteCarloVaR.map(_ - meansM(0)).map(pow(_, 2)).sum /
    (crudeMonteCarloVaR.length - 1)),
    sqrt(controlVariateVaR.map(_ - meansM(1)).map(pow(_, 2)).sum /
    (controlVariateVaR.length - 1)),
    sqrt(stratifiedMonteCarloVaR.map(_ - meansM(2)).map(pow(_, 2)).sum /
    (stratifiedMonteCarloVaR.length - 1)))

  val stdErrsM = Array(stdDevsM(0) / meansM(0), stdDevsM(1) / meansM(1),
    stdDevsM(2) / meansM(2))

  val parameterValuesM = new ArrayBuffer[Array[String]](3)

  parameterValuesM.append(Array(methods(1), doubleToString(meansM(0), 0),
    doubleToString(stdDevsM(0), 0),
    s"${doubleToString(stdErrsM(0) * 100.0, 2)}%"))
  parameterValuesM.append(Array(methods(1), doubleToString(meansM(1), 0),
    doubleToString(stdDevsM(1), 0),
    s"${doubleToString(stdErrsM(1) * 100.0, 2)}%"))
  parameterValuesM.append(Array(methods(2), doubleToString(meansM(2), 0),
    doubleToString(stdDevsM(2), 0),
    s"${doubleToString(stdErrsM(2) * 100.0, 2)}%"))

  val headerM = Array("Method", "Mean", "Std Dev", "Std Err")

  val tableM = new Table(headerM, parameterValuesM.toArray)

  // n) 1% standard error
  def sizeN(stdErr1: Double) = pow(stdErr1 / stdErr2, 2) * size1.toDouble

  val sizesN = Array(sizeN(stdErrsM(0)), sizeN(stdErrsM(1)), sizeN(stdErrsM(2)))

  val parameterValuesN = new ArrayBuffer[Array[String]](3)

  parameterValuesN.append(Array(methods(0), doubleToString(sizesN(0), 0)))
  parameterValuesN.append(Array(methods(1), doubleToString(sizesN(1), 0)))
  parameterValuesN.append(Array(methods(2), doubleToString(sizesN(2), 0)))

  val headerN = Array("Method", "Sample size")

  val tableN = new Table(headerN, parameterValuesN.toArray)

  // Displaying the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title.value = "Value-at-Risk: Exercise 10.6"

    def getHeader(str: String) = new Label {
      styleClass.add("exercise-10-6-header")

      text = str
    }

    val titles = Array("b) Realizations", "c) Quadratic remapping",
      "d) Scatter plot", "e - g) Standard deviations", "h) Standard errors",
      "i) 1% standard error", "j - l) Value-at-Risk", "m) VaR standard errors",
      "n) 1% standard error")

    val headers = new ArrayBuffer[Label]

    for (title <- titles) headers.append(getHeader(title))

    scene = new Scene {
      stylesheets = List(getClass.getResource("styles.css").toExternalForm)

      root = new GridPane

      val gridPane = new GridPane {
        styleClass.add("exercise-10-6-gridpane")
      }

      gridPane.add(headers(0), 0, 0)
      gridPane.add(tableB.getTable, 0, 1)
      gridPane.add(headers(1), 1, 0)
      gridPane.add(tableC.getTable, 1, 1)
      gridPane.add(headers(2), 2, 0)
      gridPane.add(chartD, 2, 1)
      gridPane.add(headers(3), 3, 0)
      gridPane.add(tableEFG.getTable, 3, 1)
      gridPane.add(headers(4), 4, 0)
      gridPane.add(tableH.getTable, 4, 1)
      gridPane.add(headers(5), 0, 2)
      gridPane.add(tableI.getTable, 0, 3)
      gridPane.add(headers(6), 1, 2)
      gridPane.add(tableJKL.getTable, 1, 3)
      gridPane.add(headers(7), 2, 2)
      gridPane.add(tableM.getTable, 2, 3)
      gridPane.add(headers(8), 3, 2)
      gridPane.add(tableN.getTable, 3, 3)

      content.add(gridPane)
    }
  }
}
