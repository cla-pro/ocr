package net.ocr.neural

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.FunSuite

/**
 * Created by cla on 30.07.2014.
 */
class LearningTest extends FunSuite {
  test("Test the learning process") {
    def createInputs(nbInputs: Int) = for (i <- 0 to nbInputs) yield (DenseVector[Double](i, i), if (i % 2 == 0) 1 else -1)

    val wholeInputs = createInputs(20).toList

    def checkPerformance(neuralNetwork: NeuralNetwork) =
      wholeInputs.foldLeft(0.0)((acc, elem) => acc + math.abs(neuralNetwork.execute(elem._1).output(0) - elem._2))

    def printPerformance(performance: Double) = println("Performance: " + performance)

    def iterate(inputs: List[(DenseVector[Double], Int)], neuralNetwork: NeuralNetwork): NeuralNetwork = inputs match {
      case Nil => neuralNetwork
      case (input, expected) :: xs => {
        val execution = neuralNetwork.execute(input)
        val newNeuralNetwork = execution.backPropagate(DenseVector[Double](expected))
        printPerformance(checkPerformance(newNeuralNetwork))
        iterate(xs, newNeuralNetwork)
      }
    }

    val neuralNetwork = NeuralNetwork(new DenseMatrix[Double](2, 3, Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)),
        new DenseMatrix[Double](2, 3, Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)), new DenseMatrix[Double](1, 3, Array(1.0, 1.0, 1.0)))
    printPerformance(checkPerformance(neuralNetwork))
    iterate(wholeInputs, neuralNetwork)
  }
}
