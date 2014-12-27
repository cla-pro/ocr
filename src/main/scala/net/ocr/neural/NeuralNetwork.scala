package net.ocr.neural

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Represent a neural network. once defined, some input can be applied to this network which results in an
 * execution {@link NeuralNetworkExecution} for each input.
 *
 * Created by cla on 12.06.2014.
 */
class NeuralNetwork private (_inputMatrix: DenseMatrix[Double], _middleMatrix: DenseMatrix[Double],
                             _outputMatrix: DenseMatrix[Double]) {
  val inputMatrix = _inputMatrix
  val middleMatrix = _middleMatrix
  val outputMatrix = _outputMatrix

  def execute(input: DenseVector[Double]): NeuralNetworkExecution = new NeuralNetworkExecution(this, input, transferFunction)

  val transferFunction = new TransferFunction {
    override def function: (DenseVector[Double]) => DenseVector[Double] = (x) => x

    override def derivative: (DenseVector[Double]) => DenseVector[Double] = (x) => DenseVector.ones[Double](x.length)
  }
}

object NeuralNetwork {
  def apply(inputMatrix: DenseMatrix[Double], middleMatrix: DenseMatrix[Double],
            outputMatrix: DenseMatrix[Double]) = new NeuralNetwork(inputMatrix, middleMatrix, outputMatrix)
}
