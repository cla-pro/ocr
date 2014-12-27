package net.ocr.neural

import breeze.linalg.DenseVector

/**
 * Created by cla on 30.06.2014.
 */
trait TransferFunction {
  def function: (DenseVector[Double]) => DenseVector[Double]

  def derivative: (DenseVector[Double]) => DenseVector[Double]
}
