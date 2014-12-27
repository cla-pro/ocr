package net.ocr.storage

import breeze.linalg.DenseMatrix
import net.ocr.neural.NeuralNetwork
import net.ocr.storage.JsonKeyConstant._

/**
 * Created by cla on 23.12.2014.
 */
class AlphabetFromJson {
  def alphabetFromJson(json: JsonObject): Map[Char, NeuralNetwork] =
    json.content.filter(e => e._2.isInstanceOf[JsonObject]).map(e => (e._1.charAt(0), neuralNetworkFromJson(e._2.asInstanceOf[JsonObject]))).toMap

  def neuralNetworkFromJson(json: JsonObject): NeuralNetwork = {
    val input = matrixFromJson(json(INPUT_MATRIX_KEY).asInstanceOf[JsonObject])
    val middle = matrixFromJson(json(MIDDLE_MATRIX_KEY).asInstanceOf[JsonObject])
    val output = matrixFromJson(json(OUTPUT_MATRIX_KEY).asInstanceOf[JsonObject])
    NeuralNetwork(input, middle, output)
  }

  private def matrixFromJson(json: JsonObject): DenseMatrix[Double] = {
    val rows: Int = json(WIDTH_KEY).toString.toInt
    val cols: Int = json(HEIGHT_KEY).toString.toInt
    val data: Array[Double] = json(CELLS_KEY).toString.split(' ').map(s => s.toDouble)
    new DenseMatrix[Double](rows, cols, data)
  }
}
