import java.awt.image.BufferedImage
import java.io.File
import ImageCompressor.compressImage
import javax.imageio.ImageIO
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.embed.swing.SwingFXUtils
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.{FileChooser, Screen}

object Gui extends JFXApp {
  var image: BufferedImage = _
  var compressedImage: BufferedImage = _
  val imageView = new ImageView
  val compressedImageView = new ImageView
  imageView.fitHeight = Screen.primary.bounds.height/3
  imageView.fitWidth = 3*Screen.primary.bounds.width/8
  compressedImageView.fitHeight = Screen.primary.bounds.height/3
  compressedImageView.fitWidth = 3*Screen.primary.bounds.width/8

  val file = new HBox {
    val textField = new TextField
    val fileButton = new Button("file")
    val chooseButton = new Button("Choose")
    children = List(fileButton, textField, chooseButton)
    fileButton.onAction = handle {
      val fileChooser = new FileChooser()
      val selectedFile = fileChooser.showOpenDialog(stage)
      if(selectedFile != null) textField.text = selectedFile.getAbsolutePath
    }
    chooseButton.onAction = handle {
      image = ImageIO.read(new File(textField.text.value))
      if(image != null) {
        val img = SwingFXUtils.toFXImage(image, null)
        imageView.image = img
      }
    }
  }

  val leftPane = new VBox {
    children = List(file, imageView)

  }

  val rightPane = new VBox {

    children = List(compressedImageView)
  }

  val mainPane = new HBox {
    spacing = 10
    val compressButton = new Button("Compress")
    children = List(leftPane, compressButton, rightPane)
    compressButton.onAction = handle {
      compressedImage = compressImage(image, 10, 5)
      if( compressedImage != null) {
        val img = SwingFXUtils.toFXImage(compressedImage, null)
        compressedImageView.image = img
      }
    }
  }


  stage = new JFXApp.PrimaryStage {
    title.value = "Image Compressor"
    width = 300
    height = 400
    scene = new Scene {
      fill = Color.LightGray
      content = mainPane
    }
  }
}
