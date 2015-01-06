package net.ocr.neural

import breeze.linalg.{inv, DenseVector, DenseMatrix}
import scala.math._
import org.scalatest.FunSuite

/**
 * Created by cla on 12.06.2014.
 */
class NeuralNetworkTest extends FunSuite {
  val learningRate = 0.01

  test("Full test") {
    println("Full test")

    val A = new DenseMatrix[Double](2, 3, Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
    val B = new DenseMatrix[Double](2, 3, Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
    val C = new DenseMatrix[Double](1, 3, Array(1.0, 1.0, 1.0))

    val input = DenseVector[Double](1.0, 1.0)
    val neuralNetwork = NeuralNetwork(A, B, C)
    val neuralNetworkExecution = neuralNetwork.execute(input)
    println("Result: " + neuralNetworkExecution.output)

    val expected = DenseVector[Double](4.0)
    val updatedNeuralNetwork = neuralNetworkExecution.backPropagate(expected)
    println("New A: " + updatedNeuralNetwork.inputMatrix)
    println("New B: " + updatedNeuralNetwork.middleMatrix)
    println("New C: " + updatedNeuralNetwork.outputMatrix)

    val newExecution = updatedNeuralNetwork.execute(input)
    println("New result: " + newExecution.output)
  }

//  test("test") {
//    println("Test")
//    def copy(d: DenseVector[Double]): DenseVector[Double] = {
//      val elements = for (a <- 0 to d.length - 1) yield d(a)
//      new DenseVector[Double](elements.toArray)
//    }
//
//    val transfer: (DenseVector[Double]) => DenseVector[Double] = (x) => x
//    val derivative: (DenseVector[Double]) => DenseVector[Double] = (x) => DenseVector.ones[Double](x.length)
//
//    val A = new DenseMatrix[Double](2, 2, Array(1.0, 1.0, 1.0, 1.0))
//    val B = new DenseMatrix[Double](2, 2, Array(1.0, 1.0, 1.0, 1.0))
//    val C = new DenseMatrix[Double](1, 2, Array(1.0, 1.0))
//
//    val input1 = DenseVector[Double](1.0, 1.0)
//    val outputRaw1 = A.*(input1)
//    val output1 = transfer(outputRaw1)
//
//    val input2 = copy(output1)
//    val outputRaw2 = B.*(input2)
//    val output2 = transfer(outputRaw2)
//
//    val inputFinal = copy(output2)
//    val outputRawFinal = C.*(inputFinal)
//    val outputFinal = transfer(outputRawFinal)
//    println("Result: " + outputFinal + "\n")
//
//    val expected = DenseVector[Double](4.0)
//    val diffOutput: DenseVector[Double] = expected - outputFinal
//    val gHOutput = derivative(outputRawFinal)
//    val deltaOutput = gHOutput :* (diffOutput)
//    println("Delta: " + deltaOutput)
//
//    val diffFinal: DenseVector[Double] = C.t.*(deltaOutput)
//    val gHFinal: DenseVector[Double] = derivative(outputRaw2)
//    val deltaFinal: DenseVector[Double] = gHFinal :* (diffFinal)
//    println("deltaFinal: " + deltaFinal)
//
//    val diff2: DenseVector[Double] = B.t.*(deltaFinal)
//    val gH2: DenseVector[Double] = derivative(outputRaw1)
//    val delta2: DenseVector[Double] = gH2 :* (diff2)
//    println("delta2: " + delta2)
//
//    val deltaA: DenseMatrix[Double] = delta2.toDenseMatrix.t.*(input1.toDenseMatrix) * learningRate
//    val deltaB: DenseMatrix[Double] = deltaFinal.toDenseMatrix.t.*(input2.toDenseMatrix) * learningRate
//    val deltaC: DenseMatrix[Double] = deltaOutput.toDenseMatrix.t.*(inputFinal.toDenseMatrix) * learningRate
//
//    println("DeltaA: " + deltaA)
//    println("DeltaB: " + deltaB)
//    println("DeltaC: " + deltaC)
//
//    val newA: DenseMatrix[Double] = A + deltaA
//    val newB: DenseMatrix[Double] = B + deltaB
//    val newC: DenseMatrix[Double] = C + deltaC
//
//    var newInput1 = DenseVector[Double](1.0, 1.0)
//    var newOutput1 = transfer(newA.*(newInput1))
//    var newInput2 = copy(newOutput1)
//    var newOutput2 = transfer(newB.*(newInput2))
//    var newInputFinal = copy(newOutput2)
//    var newOutputFinal = transfer(newC.*(newInputFinal))
//
//    println("New Output final: " + newOutputFinal)
//  }

  test("Real test") {
    println("Real test")
    val transferFunction = new TransferFunction {
      override def function: (DenseVector[Double]) => DenseVector[Double] = (x) => x

      override def derivative: (DenseVector[Double]) => DenseVector[Double] = (x) => DenseVector.ones[Double](x.length)
    }

    val A = new DenseMatrix[Double](2, 3, Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
    val B = new DenseMatrix[Double](2, 3, Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
    val C = new DenseMatrix[Double](1, 3, Array(1.0, 1.0, 1.0))

    val input = DenseVector[Double](1.0, 1.0)

    val step1 = new NeuralNetworkExecutionStep(A, input, transferFunction)
    val step2 = new NeuralNetworkExecutionStep(B, step1.output, transferFunction)
    val step3 = new NeuralNetworkExecutionStep(C, step2.output, transferFunction)
    val result = step3.output
    println("Result: " + result)

    val expected = DenseVector[Double](4.0)
    val outputDiff = expected - step3.output

    val backStep3 = new BackPropagationOutputStep(step3.outputRaw, step3.extendedInput, transferFunction)
    val backStep2 = new BackPropagationMiddleStep(C, step2.outputRaw, step2.extendedInput, transferFunction)
    val backStep1 = new BackPropagationMiddleStep(B, step1.outputRaw, step1.extendedInput, transferFunction)

    val deltaStep3 = backStep3.deltas(outputDiff)
    println("Delta Step3: " + deltaStep3)
    val deltaStep2 = backStep2.deltas(deltaStep3)
    println("Delta Step2: " + deltaStep2)
    val deltaStep1 = backStep1.deltas(deltaStep2)
    println("Delta Step1: " + deltaStep1)

    val deltaWeightsStep1 = backStep1.deltaWeights(deltaStep1, learningRate)
    println("Delta Weights Step1: " + deltaWeightsStep1)
    val deltaWeightsStep2 = backStep2.deltaWeights(deltaStep2, learningRate)
    println("Delta Weights Step2: " + deltaWeightsStep2)
    val deltaWeightsStep3 = backStep3.deltaWeights(deltaStep3, learningRate)
    println("Delta Weights Step3: " + deltaWeightsStep3)

    val newA = A + deltaWeightsStep1
    println("New A: " + newA)
    val newB = B + deltaWeightsStep2
    println("New B: " + newB)
    val newC = C + deltaWeightsStep3
    println("New C: " + newC)
  }

  test("Test the tangent transfer function") {
    val neural = NeuralNetwork(DenseMatrix.ones[Double](1, 2), DenseMatrix.ones[Double](1, 2), DenseMatrix.ones[Double](1, 2))

    val input1 = DenseVector[Double](Pi / 4.0, 0, Pi / 2.0, 3.0 * Pi / 4.0)
    println("Function " + neural.transferFunctionTangent.function(input1))
    println("Derivative " + neural.transferFunctionTangent.derivative(input1))
  }

  test("Test the sigmoid transfer function") {
    val neural = NeuralNetwork(DenseMatrix.ones[Double](1, 2), DenseMatrix.ones[Double](1, 2), DenseMatrix.ones[Double](1, 2))

    val input1 = DenseVector[Double](1, 0, 0.5)
    println("Function " + neural.transferFunctionSigmoid.function(input1))
    println("Derivative " + neural.transferFunctionSigmoid.derivative(input1))
  }
}
