package net.ocr.storage

import breeze.linalg.DenseMatrix
import net.ocr.neural.NeuralNetwork
import org.scalatest.FunSuite

import scala.collection.immutable.HashMap

/**
 * Created by cla on 26.12.2014.
 */
class StorageTest extends FunSuite {
  test("alphabet -> json1 -> alphabet -> json2 => json1 ?= json2") {
    val alphabet1 = createAlphabet
    val json1: JsonObject = new AlphabetToJson().alphabetToJson(alphabet1)
    val alphabet2 = new AlphabetFromJson().alphabetFromJson(json1)
    val json2: JsonObject = new AlphabetToJson().alphabetToJson(alphabet2)

    assertResult(json1)(json2)
  }

  private def createAlphabet: Map[Char, NeuralNetwork] = {
    Map('a' -> createNeuralNetwork,
      'b' -> createNeuralNetwork
    )
  }

  private def createNeuralNetwork: NeuralNetwork = {
    val A = new DenseMatrix[Double](2, 3, Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    val B = new DenseMatrix[Double](2, 3, Array(7.0, 8.0, 9.0, 10.0, 11.0, 12.0))
    val C = new DenseMatrix[Double](1, 3, Array(13.0, 14.0, 15.0))
    NeuralNetwork(A, B, C)
  }
}
