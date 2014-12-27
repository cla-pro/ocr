package net.ocr.storage

import breeze.linalg.DenseMatrix
import net.ocr.neural.NeuralNetwork
import net.ocr.storage.JsonKeyConstant._

/**
 * Created by cla on 19.12.2014.
 */
class AlphabetToJson {
  def alphabetToJson(alphabet: Map[Char, NeuralNetwork]) = {
    JsonObject(alphabet.map(e => ("" + e._1, neuralNetworkToJson(e._2))).toList)
  }

  def neuralNetworkToJson(neural: NeuralNetwork): JsonObject = {
    JsonObject((INPUT_MATRIX_KEY, matrixToJson(neural.inputMatrix)),
      (MIDDLE_MATRIX_KEY, matrixToJson(neural.middleMatrix)),
      (OUTPUT_MATRIX_KEY, matrixToJson(neural.outputMatrix)))
  }

  private def matrixToJson(matrix: DenseMatrix[Double]) = {
    JsonObject((WIDTH_KEY, matrix.rows), (HEIGHT_KEY, matrix.cols), (CELLS_KEY, matrix.data.toList.mkString("", " ", "")))
  }
}
