import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle

object Gui extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Stage"
    width = 1000
    height = 750
    scene = new Scene(400, 400) {
      val imageView = new ImageView(new Image("file:Stonehenge.jpg"))
      content = imageView
    }
  }

  stage.setMaximized(true)
}
