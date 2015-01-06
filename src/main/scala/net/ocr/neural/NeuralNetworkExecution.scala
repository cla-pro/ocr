package net.ocr.neural

import breeze.linalg.{DenseMatrix, DenseVector}
import scala.math._

/**
 * Represent a single execution of a input into the neural network. This class gather the execution information
 * in order to use them later (optionally) for the back propagation process.
 *
 * Created by cla on 18.06.2014.
 */
class NeuralNetworkExecution(neuralNetwork: NeuralNetwork, input: DenseVector[Double], transferFunction: TransferFunction) {
  private val LEARNING_RATE = 1 //pow(10, 48)

  private val inputStep = new NeuralNetworkExecutionStep(neuralNetwork.inputMatrix, input, transferFunction)
  private val middleStep = new NeuralNetworkExecutionStep(neuralNetwork.middleMatrix, inputStep.output, transferFunction)
  private val outputStep = new NeuralNetworkExecutionStep(neuralNetwork.outputMatrix, middleStep.output, transferFunction)

  val output = outputStep.output

  def backPropagate(expected: DenseVector[Double]): NeuralNetwork = {
    val outputDiff = expected - outputStep.output

    val backOutputStep = new BackPropagationOutputStep(outputStep.outputRaw, outputStep.extendedInput, transferFunction)
    val backMiddleStep = new BackPropagationMiddleStep(neuralNetwork.outputMatrix, middleStep.outputRaw, middleStep.extendedInput, transferFunction)
    val backInputStep = new BackPropagationMiddleStep(neuralNetwork.middleMatrix, inputStep.outputRaw, inputStep.extendedInput, transferFunction)

    val deltaOutputStep = backOutputStep.deltas(outputDiff)
    val deltaMiddleStep = backMiddleStep.deltas(deltaOutputStep)
    val deltaInputStep = backInputStep.deltas(deltaMiddleStep)

    val deltaInputMatrix = backInputStep.deltaWeights(deltaInputStep, LEARNING_RATE)
    val deltaMiddleMatrix = backMiddleStep.deltaWeights(deltaMiddleStep, LEARNING_RATE)
    val deltaOutputMatrix = backOutputStep.deltaWeights(deltaOutputStep, LEARNING_RATE)

    if (expected(0) == 0.9) {
      //input=$deltaInputMatrix, middle=$deltaMiddleMatrix,
      println(s"diff weights: output=$deltaOutputMatrix")
    }

    NeuralNetwork(neuralNetwork.inputMatrix + deltaInputMatrix, neuralNetwork.middleMatrix + deltaMiddleMatrix,
                    neuralNetwork.outputMatrix + deltaOutputMatrix)
  }
}
