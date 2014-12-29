package net.ocr.alphabet

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.Image
import net.ocr.image.CharImagePreprocessor
import net.ocr.neural.{NeuralNetworkExecution, NeuralNetwork}

import scala.collection.immutable.{Iterable, HashMap}

/**
 * Created by cla on 10.12.2014.
 */
class CharRecognizer(initAlphabet: Option[Map[Char, NeuralNetwork]]) {
  val alphabet: Map[Char, NeuralNetwork] = initAlphabet match {
    case None => nonTrainedAlphabet
    case Some(a) => a
  }

  def learn(image: Image, expected: Char): CharRecognizer = {
    def learnInternal = {
      val imageAsVector = new CharImagePreprocessor().prepareImage(image)
      val neuralExecutions = alphabet.mapValues(neural => neural.execute(imageAsVector))

      val positive = DenseVector(1.0)
      val negative = DenseVector(0.0)

      val upgradedAlphabet = neuralExecutions.map((e) => e._1 -> e._2.backPropagate(if (e._1 == expected) positive else negative))
      CharRecognizer(upgradedAlphabet)
    }

    if (!alphabet.contains(expected)) {
      CharRecognizer(alphabet + (expected -> initialNeuralNetwork)).learn(image, expected)
    } else {
      learnInternal
    }
  }

  def recognize(image: Image): Option[Char] = {
    println("Image size width=" + image.width + ", height=" + image.height)
    val imageAsVector = new CharImagePreprocessor().prepareImage(image)
    val neuralExecutions = alphabet.mapValues(neural => neural.execute(imageAsVector))
    neuralExecutions.toList.filter(e => e._2.output(0) > 0).sortBy(e => e._2.output(0)).reverse match {
      case Nil => None
      case x :: xs => Some(x._1)
    }
  }

  private def nonTrainedAlphabet = new HashMap[Char, NeuralNetwork] //CharRecognizer.ALPHABET_CHARACTERS.map(c => (c, initialNeuralNetwork)).toMap

  private def initialNeuralNetwork = {
    val A = DenseMatrix.ones[Double](400, 400)
    val B = DenseMatrix.ones[Double](400, 400)
    val C = DenseMatrix.ones[Double](1, 400)
    NeuralNetwork(A, B, C)
  }
}

object CharRecognizer {
  val ALPHABET_CHARACTERS = for (c <- Char.MinValue to Char.MaxValue) yield c

  def apply() = new CharRecognizer(None)
  def apply(alphabet: Map[Char, NeuralNetwork]) = new CharRecognizer(Some(alphabet))
}
