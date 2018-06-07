import java.awt.image.BufferedImage
import java.io.File

import ImageCompressor.compressImage
import javax.imageio.ImageIO
import scalafx.Includes._
import scalafx.application
import scalafx.application.JFXApp
import scalafx.embed.swing.SwingFXUtils
import scalafx.event.ActionEvent
import scalafx.geometry.{Orientation, Pos}
import scalafx.scene.{Scene, layout}
import scalafx.scene.canvas.Canvas
import scalafx.scene.control._
import scalafx.scene.image.ImageView
import scalafx.scene.layout.{BorderPane, Priority, TilePane, VBox}
import scalafx.stage.{FileChooser, Screen}

object Gui extends JFXApp {
  private var inputImage: BufferedImage = _
  private var outputImage: BufferedImage = _
  private val inputImageView = new ImageView {
    preserveRatio = true
  }
  private val outputImageView = new ImageView {
    preserveRatio = true
  }

  stage = new application.JFXApp.PrimaryStage {
    title.value = "Image Compressor"
    maximized = true
    scene = new Scene {
      val menuBar = new MenuBar
      val fileMenu = new Menu("File")
      val openItem = new MenuItem("Open")
      val saveItem = new MenuItem("Save")
      val exitItem = new MenuItem("Exit")
      fileMenu.items = List(openItem, saveItem, new SeparatorMenuItem, exitItem)

      menuBar.menus = List(fileMenu)

      val colorSlider = new Slider(1, 128, 1)
      colorSlider.minorTickCount = 62
      colorSlider.snapToTicks = true

      val colorLabel = new Label
      colorLabel.text <== colorSlider.value.asString("Colors: %.0f")

      val colorPane = new VBox
      colorPane.children ++= List( colorSlider, colorLabel)

      val iterationSlider = new Slider(1, 20, 1)
      iterationSlider.minorTickCount = 32
      iterationSlider.snapToTicks = true

      val iterationLabel = new Label
      iterationLabel.text <== iterationSlider.value.asString("Iterations: %.0f")

      val iterationPane = new VBox
      iterationPane.children ++= List(iterationSlider, iterationLabel)

      val compressButton = new Button("COMPRESS")
      compressButton.text

      val bottomPane = new TilePane
      bottomPane.prefTileWidth <== width * 0.3
      bottomPane.hgap = 5
      bottomPane.children ++= List(colorPane, iterationPane, compressButton)

      val leftPane = new BorderPane
      leftPane.center = inputImageView
      inputImageView.fitWidth <== width * 0.45
      inputImageView.fitHeight <== height * 0.75

      val rightPane = new BorderPane
      rightPane.center = outputImageView
      outputImageView.fitWidth <== width * 0.45
      outputImageView.fitHeight <== height * 0.75

      compressButton.onAction = (ae: ActionEvent) => {
        if(inputImage != null){
          outputImage = compressImage(inputImage, colorSlider.value.toInt, iterationSlider.value.toInt)
          outputImageView.image = SwingFXUtils.toFXImage(outputImage, null)
        }
      }

      openItem.onAction = (ae: ActionEvent) => {
        val fileChooser = new FileChooser
        val selectedFile = fileChooser.showOpenDialog(stage)
        if (selectedFile != null) {
          inputImage = ImageIO.read(new File(selectedFile.getAbsolutePath))
          inputImageView.image = SwingFXUtils.toFXImage(inputImage, null)
        }
      }
      saveItem.onAction = (ae: ActionEvent) => {
        if (outputImage != null) {
          val fileChooser = new FileChooser
          val selectedFile = fileChooser.showOpenDialog(stage)
          if (selectedFile != null)
            ImageIO.write(outputImage, "jpg", new File(selectedFile.getAbsolutePath))
        }
      }
      exitItem.onAction = (ae: ActionEvent) => {
        sys.exit(0)
      }

      val rootPane = new BorderPane
      rootPane.top = menuBar
      rootPane.left = leftPane
      rootPane.right = rightPane
      rootPane.bottom = bottomPane
      root = rootPane
    }
  }

}
