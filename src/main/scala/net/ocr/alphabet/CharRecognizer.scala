package net.ocr.alphabet

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.Image
import net.ocr.image.CharImagePreprocessor
import net.ocr.neural.{NeuralNetworkExecution, NeuralNetwork}

import scala.collection.immutable.{Iterable, HashMap}

/**
 * Created by cla on 10.12.2014.
 */
class CharRecognizer(initAlphabet: Option[Map[String, NeuralNetwork]]) {
  val alphabet: Map[String, NeuralNetwork] = initAlphabet match {
    case None => nonTrainedAlphabet
    case Some(a) => a
  }

  def initWithChars(chars: List[String]): CharRecognizer = CharRecognizer(chars.map(c => (c, initialNeuralNetwork)).toMap)

  def learn(image: Image, expected: String): CharRecognizer = {
    def learnInternal = {
      val imageAsVector = new CharImagePreprocessor().prepareImage(image)
      val neuralExecutions = alphabet.mapValues(neural => neural.execute(imageAsVector))

      val positive = DenseVector(0.9)
      val negative = DenseVector(0.1)

      println("5 tops: "
        + neuralExecutions.toList.map(e => (e._1, e._2.output(0) - (if (e._1 == expected) positive(0) else negative(0)))).sortBy(e => e._2).slice(0, 5))
      //neuralExecutions.foreach(e => println("Value: " + e._1 + " -> " + e._2.output))

      val upgradedAlphabet = neuralExecutions.map((e) => e._1 -> e._2.backPropagate(if (e._1 == expected) positive else negative))
      //println("Upgraded G=" + upgradedAlphabet("G"))

      val secondRun = upgradedAlphabet.mapValues(neural => neural.execute(imageAsVector))
      //secondRun.foreach(e => println("Value: " + e._1 + " -> " + e._2.output))

      println("Upgraded 5 tops: "
        + neuralExecutions.toList.map(e => (e._1, e._2.output(0) - (if (e._1 == expected) positive(0) else negative(0)))).sortBy(e => e._2).slice(0, 5))

      CharRecognizer(upgradedAlphabet)
    }

    if (!alphabet.contains(expected)) {
      println("Unknown expected chain \"" + expected + "\" -> creating an initial neural network")
      CharRecognizer(alphabet + (expected -> initialNeuralNetwork)).learn(image, expected)
    } else {
      learnInternal
    }
  }

  def recognize(image: Image): Option[String] = {
    val imageAsVector = new CharImagePreprocessor().prepareImage(image)
    val neuralExecutions = alphabet.mapValues(neural => neural.execute(imageAsVector))
    val matches: List[(String, NeuralNetworkExecution)] = neuralExecutions.toList.filter(e => e._2.output(0) > 0).sortBy(e => e._2.output(0)).reverse
    println("Positive matches: " + matches.length)
    matches match {
      case Nil => None
      case x :: xs => Some(x._1)
    }
  }

  private def nonTrainedAlphabet = new HashMap[String, NeuralNetwork] //CharRecognizer.ALPHABET_CHARACTERS.map(c => (c, initialNeuralNetwork)).toMap

  private def initialNeuralNetwork = {
    // 20 x 20 (image size) + 1 (for the additional neuron)
    val A = DenseMatrix.ones[Double](400, 401)
    val B = DenseMatrix.ones[Double](400, 401)
    val C = DenseMatrix.ones[Double](1, 401)
    NeuralNetwork(A, B, C)
  }
}

object CharRecognizer {
  val ALPHABET_CHARACTERS = for (c <- Char.MinValue to Char.MaxValue) yield c

  def apply() = new CharRecognizer(None)
  def apply(alphabet: Map[String, NeuralNetwork]) = new CharRecognizer(Some(alphabet))
}
