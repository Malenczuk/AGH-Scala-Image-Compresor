import java.io.File
import javax.imageio.ImageIO
import ImageCompressor.compressImage

object Appl {

  def main(args: Array[String]): Unit = {
    val image = ImageIO.read(new File(args(0)))
    println("Photo size is " + image.getWidth + " x " + image.getHeight + "\n")

    val newImage = compressImage(image, 6, 5)


    ImageIO.write(newImage, "jpg", new File("compressedImage.jpg"))
  }
}


