package net.ocr.neural

import breeze.linalg.{DenseVector, DenseMatrix}
import scala.math._

/**
 * Represent a neural network. once defined, some input can be applied to this network which results in an
 * execution {@link NeuralNetworkExecution} for each input.
 *
 * Created by cla on 12.06.2014.
 */
class NeuralNetwork private (_inputMatrix: DenseMatrix[Double], _middleMatrix: DenseMatrix[Double],
                             _outputMatrix: DenseMatrix[Double]) {
  var inputMatrix = _inputMatrix
  var middleMatrix = _middleMatrix
  var outputMatrix = _outputMatrix

  def execute(input: DenseVector[Double]): NeuralNetworkExecution = new NeuralNetworkExecution(this, input, transferFunctionSigmoid)

  val transferFunctionLinear = new TransferFunction {
    override def function: (DenseVector[Double]) => DenseVector[Double] = (x) => x

    override def derivative: (DenseVector[Double]) => DenseVector[Double] = (x) => DenseVector.ones[Double](x.length)
  }

  val transferFunctionTangent = new TransferFunction {
    override def function: (DenseVector[Double]) => DenseVector[Double] = (x) => DenseVector[Double](x.data.map(v => tan(v)))

    override def derivative: (DenseVector[Double]) => DenseVector[Double] = (x) => DenseVector[Double](x.data.map(v => 1.0 / (cos(v) * cos(v))))
  }

  val transferFunctionSigmoid = new TransferFunction {
    override def function: (DenseVector[Double]) => DenseVector[Double] = (x) => DenseVector[Double](x.data.map(v => 1.0 / (1.0 + exp(-v))))

    override def derivative: (DenseVector[Double]) => DenseVector[Double] = (x) => DenseVector[Double](x.data.map(v =>
      if (abs(v) > 370) 0
      else exp(-v) / pow(exp(-v) + 1.0, 2))
    )
  }

  override def toString: String = s"Neural[input=$inputMatrix, middle=$middleMatrix, output=$outputMatrix]"
}

object NeuralNetwork {
  def apply(inputMatrix: DenseMatrix[Double], middleMatrix: DenseMatrix[Double],
            outputMatrix: DenseMatrix[Double]) = new NeuralNetwork(inputMatrix, middleMatrix, outputMatrix)
}
