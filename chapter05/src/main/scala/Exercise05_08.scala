package valueatrisk.chapter05

import utils._

import scala.collection.mutable.ArrayBuffer
import scala.math.{exp, log, min, max, pow, sqrt, cbrt}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property.DoubleProperty
import scalafx.geometry.Orientation
import scalafx.scene.control.Label
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.ScrollEvent
import scalafx.scene.layout.{StackPane, TilePane, VBox}
import scalafx.scene.PerspectiveCamera
import scalafx.scene.Scene

import scalafx.scene.paint.Color
import scalafx.scene.transform.Rotate

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.mllib.linalg._
import org.apache.spark.mllib.random._
import org.apache.spark.mllib.stat.Statistics

import org.apache.commons.math3.distribution.NormalDistribution

import breeze.linalg.DenseMatrix

/** Proposed solution for exercise 5.8 in the book "Value-at-Risk:
 *  Theory and Practice" (2nd edition) by Glyn A. Holton.
 */
object Exercise05_08 extends JFXApp {
  val size = 400 // Chart axes
  val amplitude = 400 // Surface amplitude, should be less than size

  val cubeColor = Color.DarkCyan // Chart background
  val gridColor = Color.White // Chart background grid
  val meshColor1 = Color.Red // Chart surface
  val meshColor2 = Color.Yellow

  // Initial values for animations
  private var mousePosX = 0.0
  private var mousePosY = 0.0

  private var mouseOldX = 0.0
  private var mouseOldY = 0.0

  private final val rotateX = new Rotate(60, Rotate.XAxis)
  private final val rotateY = new Rotate(120, Rotate.YAxis)

  /** Returns the value of a function for samples of two pseudorandom variables.
   *
   *  @param f the function
   *  @param ctx a valid Spark context
   *  @param sampleSize the sample size
   */
  def randomVector(context: SparkContext, sampleSize: Int) = {
    val generator = new UniformGenerator

    val sample = new ArrayBuffer[Double]

    for (i <- 1 to sampleSize) sample += generator.nextValue

    sample
  }

  // Connecting to Apache Spark
  val conf: SparkConf =
    new SparkConf()
     .setMaster("local[*]")
     .setAppName("Exercise 5.4")
     .set("spark.driver.host", "localhost")

  val sc = new SparkContext(conf)

  sc.setLogLevel("WARN")

  // a) Change of variables
  val u1Values = (0 to size).map(_ / size.toDouble).toArray
  val u2Values = (0 to size).map(_ / size.toDouble).toArray

  def aFunc = (u1: Double, u2: Double) => (4.0 * exp(cbrt(2 * u1 - 1) +
                                           cbrt(2 * u2 - 1))).toFloat

  // d) Stratifying sample
  val solutionD = new Label {
    wrapText = true
    val sd1 = "d) The graph to the right shows that region (u1 < 0.5, u2 < 0.5)"
    val sd2 = " has the lowest variance and region (u1 > 0.5, u2 > 0.5) has the"
    val sd3 = " highest variance."

    text = sd1 + sd2 + sd3
  }

  // e) Estimating mean and standard deviations for pseudo-random vectors
  val eSampleSize = 50

  val y11 = randomVector(sc, eSampleSize).map(_ * 0.5) // Omega 1
  val y12 = randomVector(sc, eSampleSize).map(_ * 0.5)
  val y21 = randomVector(sc, eSampleSize).map(_ * 0.5) // Omega 2
  val y22 = randomVector(sc, eSampleSize).map(_ * 0.5 + 0.5)
  val y31 = randomVector(sc, eSampleSize).map(_ * 0.5 + 0.5)  // Omega 3
  val y32 = randomVector(sc, eSampleSize).map(_ * 0.5 + 0.5)

  val y1 = new ArrayBuffer[Double]
  val y2 = new ArrayBuffer[Double]
  val y3 = new ArrayBuffer[Double]

  for (i <- 0 to eSampleSize - 1) {
    y1 += aFunc(y11(i), y12(i))
    y2 += aFunc(y21(i), y22(i))
    y3 += aFunc(y31(i), y32(i))
  }

  val mean1 = y1.reduce(_ + _) / y1.length
  val mean2 = y2.reduce(_ + _) / y2.length
  val mean3 = y3.reduce(_ + _) / y3.length

  val stdDev1 = sqrt(y1.map(_ - mean1).map(pow(_, 2)).reduce(_ + _) /
    (y1.length - 1))
  val stdDev2 = sqrt(y2.map(_ - mean2).map(pow(_, 2)).reduce(_ + _) /
    (y2.length - 1))
  val stdDev3 = sqrt(y3.map(_ - mean3).map(pow(_, 2)).reduce(_ + _) /
    (y3.length - 1))

  val solutionE = new Label {
    wrapText = true

    val se1 = s"e) Means: ${rounded(mean1, 3)}, ${rounded(mean2, 3)} and "
    val se2 = s"${rounded(mean3, 3)}. Standard deviations: "
    val se3 = s"${rounded(stdDev1, 3)}, ${rounded(stdDev2, 3)} and "
    val se4 = s"${rounded(stdDev3, 3)}."

    text = se1 + se2 + se3 + se4
  }

  // f) Suitable sample sizes
  val m = 1000

  val p1 = 0.25
  val p2 = 0.50
  val p3 = 0.25

  val sSum = p1 * stdDev1 + p2 * stdDev2 + p3 * stdDev3

  val sampleSize1 = rounded(m * p1 * stdDev1 / sSum, 0)
  val sampleSize2 = rounded(m * p2 * stdDev2 / sSum, 0)
  val sampleSize3 = rounded(m * p3 * stdDev3 / sSum, 0)

  val solutionF = new Label {
    wrapText = true

    val sf1 = s"f) Suitable sample sizes: ${doubleToString(sampleSize1, 0)}, "
    val sf2 = s"${doubleToString(sampleSize2, 0)} and "
    val sf3 = s"${doubleToString(sampleSize3, 0)}."

    text = sf1 + sf2 + sf3
  }

  // i) Computing standard errors
  val seEstimator = sqrt(pow(p1 * stdDev1, 2) / sampleSize1 +
                    pow(p2 * stdDev2, 2) / sampleSize2 + pow(p3 * stdDev3, 2) /
                    sampleSize3)

  val crudeMean = p1 * mean1 + p2 * mean2 + p3 * mean3
  val crudeStdDev = sqrt(p1 * (pow(stdDev1, 2) + pow(mean1, 2)) + p2 *
                (pow(stdDev2, 2) + pow(mean2, 2)) + p3 * (pow(stdDev3, 2) +
                pow(mean3, 2)) - pow(crudeMean, 2))
  val seCrude = crudeStdDev / sqrt(m)

  val solutionI = new Label {
    wrapText = true

    val si1 = s"i) Stratified estimator standard error: "
    val si2 = s"${rounded(seEstimator, 4)}. Crude estimator standard error: "
    val si3 = s"${rounded(seCrude, 4)}"

    text = si1 + si2 + si3
  }

  // j) Sample size for reducing crude estimator standard error

  val newSize = pow(crudeStdDev / seEstimator, 2).toInt + 1

  val solutionJ = new Label {
    wrapText = true

    text = s"j) Sample size would have to be ${newSize}."
  }

  // k) Estimating the integral
  val e11 = randomVector(sc, sampleSize1.toInt).map(_ * 0.5) // Omega 1
  val e12 = randomVector(sc, sampleSize1.toInt).map(_ * 0.5)
  val e21 = randomVector(sc, sampleSize2.toInt).map(_ * 0.5) // Omega 2
  val e22 = randomVector(sc, sampleSize2.toInt).map(_ * 0.5 + 0.5)
  val e31 = randomVector(sc, sampleSize3.toInt).map(_ * 0.5 + 0.5)  // Omega 3
  val e32 = randomVector(sc, sampleSize3.toInt).map(_ * 0.5 + 0.5)

  val e1 = new ArrayBuffer[Double]
  val e2 = new ArrayBuffer[Double]
  val e3 = new ArrayBuffer[Double]

  for (i <- 0 to sampleSize1.toInt - 1) {
    e1 += aFunc(e11(i), e12(i))
  }
  for (i <- 0 to sampleSize2.toInt - 1) {
    e2 += aFunc(e21(i), e22(i))
  }
  for (i <- 0 to sampleSize3.toInt - 1) {
    e3 += aFunc(e31(i), e32(i))
  }

  val eMean = p1 * e1.reduce(_ + _) / sampleSize1 + p2 * e2.reduce(_ + _) /
              sampleSize2 + p3 * e3.reduce(_ + _) / sampleSize3

  val solutionK = new Label {
    wrapText = true

    text = s"k) Estimated value: ${rounded(eMean, 3)}."
  }

  // Building the graphical user interface
  stage = new JFXApp.PrimaryStage {
    title = "Value-at-Risk: Exercise 5.8"

    scene = new Scene(1200, 680) {
      stylesheets = List(getClass.getResource("styles.css").toExternalForm)

      camera = new PerspectiveCamera

      // Mouse drag and drop is used for rotating the graph
      onMousePressed = (me: MouseEvent) => {
        mouseOldX = me.sceneX
        mouseOldY = me.sceneY
      }
      onMouseDragged = (me: MouseEvent) => {
        mousePosX = me.sceneX
        mousePosY = me.sceneY
        rotateX.angle = rotateX.getAngle - (mousePosY - mouseOldY)
        rotateY.angle = rotateY.getAngle + (mousePosX - mouseOldX)
        mouseOldX = mousePosX
        mouseOldY = mousePosY
      }

      // Creating a graph surface and background cube
      val surface = new Surface(aFunc, u1Values, u2Values, amplitude,
                                meshColor1, meshColor2)

      val cube = new Cube(size, cubeColor, gridColor) {
        children.add(surface.getMeshView)
        transforms.addAll(rotateX, rotateY)
      }

      val stackPane = new StackPane {
        // Centring the graph in the window
        translateX = 50
        translateY = 150

        // Zoom limits
        val MAX_SCALE = 10.0
        val MIN_SCALE = 0.1

        // Scroll is used for zooming in and out
        onScroll = (event: ScrollEvent) => {
          if (event.eventType == ScrollEvent.Scroll) {
            val delta = 1.2
            var scale = scaleX.toDouble

            if (event.deltaY < 0) scale /= delta
            else scale *= delta

            scale = min(max(scale, MIN_SCALE), MAX_SCALE)

            scaleX = scale
            scaleY = scale

            event.consume
          }
        }

        children.add(cube)
      }

      val vBox = new VBox {
        styleClass.add("vbox")
        maxWidth = 500

        children.addAll(solutionD, solutionE, solutionF, solutionI, solutionJ,
                        solutionK)
      }

      root = new TilePane {
        styleClass.add("exercise-5-8")

        orientation = Orientation.Vertical
        children.add(vBox)
        children.add(stackPane)
      }
    }
  }
}
