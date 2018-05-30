import java.io.File
import javax.imageio.ImageIO
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
      centroidMatrix(i,0) = matrix(centroids(i), 0)
      centroidMatrix(i,1) = matrix(centroids(i), 1)
      centroidMatrix(i,2) = matrix(centroids(i), 2)
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

    var newCentroids = DenseMatrix.zeros[Double](max(idx)+1, 3)

    for (i <- 0 until max(idx)+1 ) {
      val oneCentroidPixels = idx.findAll((v: Int) => v == i)

      val A = DenseMatrix.zeros[Int](matrix.rows, 3)

      for (i <- oneCentroidPixels){
        A(i, 0) = matrix(i, 0)
        A(i, 1) = matrix(i, 1)
        A(i, 2) = matrix(i, 2)
      }

      var B = sum(A(::, *))

      B = B :/= oneCentroidPixels.length

      newCentroids(i, 0) = B(0)
      newCentroids(i, 1) = B(1)
      newCentroids(i, 2) = B(2)
    }

    newCentroids.mapValues((v: Double) => Math.round(v.floatValue))
  }

  def runKMeans(matrix: DenseMatrix[Int], centroidNumber: Int, maxIter: Int): DenseMatrix[Int] ={
    var centroids = initRandomCentroids(matrix,centroidNumber, matrix.rows)
//    println(centroids)
//    println()

    for (i <- 0 until maxIter){
      var idx = findClosestCentroids(matrix, centroids)
      centroids = computeCentroids(matrix, idx)
//      println(centroids, "\n")
//      println()

    }
    centroids
  }

  def projectColors(matrix: DenseMatrix[Int], centroids: DenseMatrix[Int]): DenseMatrix[Int] = {
    var projectedMatrix = matrix.copy
    for (i <- 0 until matrix.rows) {
      var A = centroids(*, ::) - matrix(i, ::).t
      A = A *:* A
      val B = sum(A(*, ::))
      val closestCentroid = argmin(B)
      projectedMatrix(i, 0) = centroids(closestCentroid, 0)
      projectedMatrix(i, 1) = centroids(closestCentroid, 1)
      projectedMatrix(i, 2) = centroids(closestCentroid, 2)
    }

    projectedMatrix
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


    val finalCentroids = runKMeans(matrix, 10, 5)
    println(finalCentroids)

    val newImage = projectColors(matrix, finalCentroids)
    println(newImage(1,::))
    println(newImage(100,::))
    println(newImage(202,::))
    println(newImage(900,::))
    println(newImage(1000,::))
    println(newImage(1500,::))
    println(newImage(2000,::))
    println(newImage(2500,::))
    println(newImage(3000,::))

}}


