package net.ocr.neural

import breeze.linalg.{DenseMatrix, DenseVector}

class NeuralNetworkExecutionStep(weights: DenseMatrix[Double], input: DenseVector[Double],
                                  transferFunction: TransferFunction) {
  val extendedInput = extendInput(input)
  val outputRaw = weights * extendedInput
  val output = transferFunction.function(outputRaw)

  def extendInput(toExtend: DenseVector[Double]) = {
    val extended = DenseVector.zeros[Double](toExtend.length + 1)
    extended(0 to toExtend.length - 1) := toExtend
    extended(toExtend.length) = -1.0
    extended
  }
}

class BackPropagationMiddleStep(weights: DenseMatrix[Double], outputRaw: DenseVector[Double],
                                input: DenseVector[Double], transferFunction: TransferFunction) {
  def deltas(nextLevelDelta: DenseVector[Double]): DenseVector[Double] = {
    val diff: DenseVector[Double] = weights.t.*(nextLevelDelta)
    val derivativeOutputRaw: DenseVector[Double] = transferFunction.derivative(outputRaw)
    val delta: DenseVector[Double] = derivativeOutputRaw:*(diff)
    delta
  }

  def deltaWeights(deltas: DenseVector[Double], learningRate: Double): DenseMatrix[Double] = {
    val deltasTranspos: DenseMatrix[Double] = deltas.toDenseMatrix.t
    deltasTranspos.*(input.toDenseMatrix) :* learningRate
  }
}

class BackPropagationOutputStep(outputRaw: DenseVector[Double], input: DenseVector[Double],
                                     transferFunction: TransferFunction) {
  def deltas(outputDelta: DenseVector[Double]): DenseVector[Double] = {
    val derivativeOutputRaw: DenseVector[Double] = transferFunction.derivative(outputRaw)
    val delta: DenseVector[Double] = derivativeOutputRaw:*(outputDelta)
    delta
  }

  def deltaWeights(deltas: DenseVector[Double], learningRate: Double): DenseMatrix[Double] = deltas.toDenseMatrix.t.*(input.toDenseMatrix) * learningRate
}
