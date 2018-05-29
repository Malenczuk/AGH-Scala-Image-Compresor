import java.io.File
import javax.imageio.ImageIO
import breeze.linalg._


object Appl {
  def main(args: Array[String]): Unit = {
    val photo = ImageIO.read(new File(args(0)))
    println("Photo size is " + photo.getWidth + " x " + photo.getHeight + "\n")

    val pixels = for (y <- 0 until photo.getHeight; x <- 0 until photo.getWidth) yield
                     ((photo.getRGB(x, y) & 0xff0000) / 65536,
                       (photo.getRGB(x, y) & 0x00ff00) / 256,
                       photo.getRGB(x, y) & 0x0000ff)

    print(pixels(0))
  }
}