import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import breeze.linalg._


object Appl {

  def initRandomCentroids(matrix: DenseMatrix[Int], centroidNumber: Int, pixelCount: Int): DenseMatrix[Int] = {
    val randGen = scala.util.Random
    val centroids = new Array[Int](centroidNumber)

    for (i <- 0 until centroidNumber){
      var x = randGen.nextInt(pixelCount)
      while (centroids.contains(x)){
        x = randGen.nextInt(pixelCount)
      }
      centroids(i) = x
    }

    var centroidMatrix = DenseMatrix.zeros[Int](centroidNumber, 3)

    for (i <- 0 until centroidNumber){
      centroidMatrix(i,::) :+= matrix(centroids(i), ::)
    }

    centroidMatrix
  }


  def findClosestCentroids(matrix: DenseMatrix[Int], centroids: DenseMatrix[Int]): DenseVector[Int] = {
    val idx = DenseVector.zeros[Int](matrix.rows)
    for (i <- 0 until matrix.rows){
      var A = centroids(*, ::) - matrix(i, ::).t
      A = A *:* A
      val B = sum(A(*, ::))
      idx(i) = argmin(B)
    }
    idx
  }


  def computeCentroids(matrix: DenseMatrix[Int], idx: DenseVector[Int]): DenseMatrix[Int] = {

    val newCentroids = DenseMatrix.zeros[Double](max(idx)+1, 3)

    for (i <- 0 until max(idx)+1 ) {
      val oneCentroidPixels = idx.findAll((v: Int) => v == i)

      val A = DenseMatrix.zeros[Int](matrix.rows, 3)

      for (i <- oneCentroidPixels){
        A(i, ::) :+= matrix(i, ::)
      }

      val B = sum(A(::, *)).t
      B :/= oneCentroidPixels.length

      newCentroids(i, 0) =  B(0)
      newCentroids(i, 1) = B(1)
      newCentroids(i, 2) = B(2)
    }

    newCentroids.mapValues((v: Double) => Math.round(v.floatValue))
  }

  def runKMeans(matrix: DenseMatrix[Int], centroidNumber: Int, maxIter: Int): DenseMatrix[Int] ={
    var centroids = initRandomCentroids(matrix,centroidNumber, matrix.rows)
    for (i <- 0 until maxIter){
      val idx = findClosestCentroids(matrix, centroids)
      centroids = computeCentroids(matrix, idx)
    }
    centroids
  }

  def projectColors(matrix: DenseMatrix[Int], centroids: DenseMatrix[Int]): DenseMatrix[Int] = {
    val projectedMatrix: DenseMatrix[Int] = DenseMatrix.zeros[Int](matrix.rows, 3)
    for (i <- 0 until matrix.rows) {
      val A = centroids(*, ::) - matrix(i, ::).t
      A :*= A
      val B = sum(A(*, ::))
      val closestCentroid = argmin(B)
      projectedMatrix(i, ::) :+= centroids(closestCentroid, ::)
    }

    projectedMatrix
  }

  def saveImage(matrix: DenseMatrix[Int], filename: String, height: Int, width: Int): Unit = {
    val newImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for (x <- 0 until width)
      for (y <- 0 until height){
        val rgbValue = (matrix(x+y*width, 0) * 65536 + matrix(x+y*width, 1) * 256 + matrix(x+y*width, 2)) & 0xffffff
        newImage.setRGB(x, y, rgbValue)
      }

    ImageIO.write(newImage, "jpg", new File(filename))
  }


  def main(args: Array[String]): Unit = {
    val photo = ImageIO.read(new File(args(0)))
    println("Photo size is " + photo.getWidth + " x " + photo.getHeight + "\n")

    val matrix = DenseMatrix.zeros[Int](photo.getWidth * photo.getHeight, 3)

    for (y <- 0 until photo.getHeight; x <- 0 until photo.getWidth) {
      matrix(x + y * photo.getWidth, 0) = (photo.getRGB(x, y) & 0xff0000) / 65536
      matrix(x + y * photo.getWidth, 1) = (photo.getRGB(x, y) & 0x00ff00) / 256
      matrix(x + y * photo.getWidth, 2) = photo.getRGB(x, y) & 0x0000ff
    }


    val finalCentroids = runKMeans(matrix, 6, 5)
    println(finalCentroids)

    val newImage = projectColors(matrix, finalCentroids)

    saveImage(newImage, "compressedImage.jpg", photo.getHeight, photo.getWidth)

  }
}


