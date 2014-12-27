package net.ocr.storage

import breeze.linalg.DenseMatrix
import net.ocr.neural.NeuralNetwork
import net.ocr.storage.JsonKeyConstant._
import org.scalatest.FunSuite

/**
 * Created by cla on 22.12.2014.
 */
class AlphabetToJsonTest extends FunSuite {
  test("Test neuralNetworkToJson") {
    val A = new DenseMatrix[Double](2, 3, Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    val B = new DenseMatrix[Double](2, 3, Array(7.0, 8.0, 9.0, 10.0, 11.0, 12.0))
    val C = new DenseMatrix[Double](1, 3, Array(13.0, 14.0, 15.0))
    val neuralNetwork = NeuralNetwork(A, B, C)

    val json: JsonObject = new AlphabetToJson().neuralNetworkToJson(neuralNetwork)
    val expected: JsonObject = JsonObject(
      (INPUT_MATRIX_KEY -> JsonObject((WIDTH_KEY -> 2), (HEIGHT_KEY -> 3), (CELLS_KEY, "1.0 2.0 3.0 4.0 5.0 6.0"))),
      (MIDDLE_MATRIX_KEY -> JsonObject((WIDTH_KEY -> 2), (HEIGHT_KEY -> 3), (CELLS_KEY, "7.0 8.0 9.0 10.0 11.0 12.0"))),
      (OUTPUT_MATRIX_KEY -> JsonObject((WIDTH_KEY -> 1), (HEIGHT_KEY -> 3), (CELLS_KEY, "13.0 14.0 15.0"))))
    assertResult(expected)(json)
  }
}
