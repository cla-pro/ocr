package net.ocr.image

import breeze.linalg.DenseVector
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.filter.GaussianBlurFilter

/**
 * Created by cla on 31.07.2014.
 */
class CharImagePreprocessor {
  val CHAR_WIDTH: Int = 20
  val CHAR_HEIGHT: Int = 20

  def prepareImage(image: Image) = imageToVector(resizeAndBlurImage(image))

  private def imageToVector(image: Image) = {
    val pixels = for (i <- 0 to image.height - 1; j <- 0 to image.width - 1) yield image.pixel(j, i).toDouble
    val pixelsArray = pixels.toArray
    DenseVector[Double](pixelsArray)
  }

  private def resizeAndBlurImage(image: Image) = image.filter(new GaussianBlurFilter(1)).scaleTo(CHAR_WIDTH, CHAR_HEIGHT)
}
