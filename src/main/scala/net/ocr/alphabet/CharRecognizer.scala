package net.ocr.alphabet

import java.util.Map.Entry

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.Image
import net.ocr.image.CharImagePreprocessor
import net.ocr.neural.{NeuralNetworkExecution, NeuralNetwork}

import scala.collection.immutable.{Iterable, HashMap}

/**
 * Created by cla on 10.12.2014.
 */
abstract class CharRecognizer {
  def initWithChars(chars: List[String]): CharRecognizer

  def learn(image: Image, expected: String): CharRecognizer

  def recognize(image: Image): Option[String]

  def resetDurations

  def displayDurations

  def alphabet: Map[String, NeuralNetwork] = new HashMap[String, NeuralNetwork]
}


class JavaCharRecognizer(initAlphabet: Option[java.util.Map[String, NeuralNetwork]]) extends CharRecognizer {
  var durationImage: Long = 0L
  var durationExecution: Long = 0L
  var durationExecutionMap: Long = 0L
  var durationBackpropagate: Long = 0L
  var durationBackpropagateMap: Long = 0L

  val internalAlphabet: java.util.Map[String, NeuralNetwork] = initAlphabet match {
    case None => nonTrainedAlphabet
    case Some(a) => a
  }

  def initWithChars(chars: List[String]): CharRecognizer = {
    import scala.collection.JavaConversions._
    chars.foreach(c => internalAlphabet.put(c, initialNeuralNetwork))
    this
  }

  def learn(image: Image, expected: String): CharRecognizer = {
    def learnInternal = {
      val startTimeImage = System.currentTimeMillis()
      val imageAsVector = new CharImagePreprocessor().prepareImage(image)
      durationImage += (System.currentTimeMillis() - startTimeImage)

      //val neuralExecutions = alphabet.mapValues(neural => neural.execute(imageAsVector))
      val neuralExecutions: java.util.Map[String, NeuralNetworkExecution] = new java.util.HashMap[String, NeuralNetworkExecution]
      val alphabetIterator = internalAlphabet.entrySet.iterator
      while (alphabetIterator.hasNext) {
        val entry = alphabetIterator.next()

        val startTimeExecution = System.currentTimeMillis()
        val execution: NeuralNetworkExecution = entry.getValue.execute(imageAsVector)
        durationExecution += (System.currentTimeMillis() - startTimeExecution)

        val startTimeExecutionMap = System.currentTimeMillis()
        neuralExecutions.put(entry.getKey, execution)
        durationExecutionMap += (System.currentTimeMillis() - startTimeExecutionMap)
      }

      val positive = DenseVector(0.9)
      val negative = DenseVector(0.1)

      //val upgradedAlphabet = neuralExecutions.map((e) => e._1 -> e._2.backPropagate(if (e._1 == expected) positive else negative))
      //val upgradedAlphabet = neuralExecutions.mapValues(e => (e._1, e._2.backPropagate(if (e._1 == expected) positive else negative)))
      val executionIterator: java.util.Iterator[Entry[String, NeuralNetworkExecution]] = neuralExecutions.entrySet().iterator()
      while (executionIterator.hasNext) {
        val entry: Entry[String, NeuralNetworkExecution] = executionIterator.next()

        val startTimeBackpropagate = System.currentTimeMillis()
        val propagate: NeuralNetwork = entry.getValue.backPropagate(if (entry.getKey == expected) positive else negative)
        durationBackpropagate += (System.currentTimeMillis() - startTimeBackpropagate)

        val startTimeBackpropagateMap = System.currentTimeMillis()
        internalAlphabet.put(entry.getKey, propagate)
        durationBackpropagateMap += (System.currentTimeMillis() - startTimeBackpropagateMap)
      }

      this
    }

    if (!internalAlphabet.containsKey(expected)) {
      println("Unknown expected chain \"" + expected + "\" -> creating an initial neural network")
      internalAlphabet.put(expected, initialNeuralNetwork)
      CharRecognizer(internalAlphabet).learn(image, expected)
    } else {
      learnInternal
    }
  }

  def recognize(image: Image): Option[String] = {
    import scala.collection.JavaConversions._
    val imageAsVector = new CharImagePreprocessor().prepareImage(image)
    val neuralExecutions = internalAlphabet.mapValues(neural => neural.execute(imageAsVector))
    val matches: List[(String, NeuralNetworkExecution)] = neuralExecutions.toList.filter(e => e._2.output(0) > 0).sortBy(e => e._2.output(0)).reverse
    //println("Positive matches: " + matches.length)
    matches match {
      case Nil => None
      case x :: xs => Some(x._1)
    }
  }

  private def nonTrainedAlphabet = new java.util.HashMap[String, NeuralNetwork] //CharRecognizer.ALPHABET_CHARACTERS.map(c => (c, initialNeuralNetwork)).toMap

  private def initialNeuralNetwork = {
    // 20 x 20 (image size) + 1 (for the additional neuron)
    val A = DenseMatrix.rand[Double](150, 401)
    val B = DenseMatrix.rand[Double](50, 151)
    val C = DenseMatrix.rand[Double](1, 51)
    NeuralNetwork(A, B, C)
  }

  def resetDurations {
    durationImage = 0L
    durationExecution = 0L
    durationExecutionMap = 0L
    durationBackpropagate = 0L
    durationBackpropagateMap = 0L
  }

  def displayDurations = println(s"Duration: image=${durationImage} " +
    s"execution=${durationExecution} executionMap=${durationExecutionMap} " +
    s"back=${durationBackpropagate} back=${durationBackpropagateMap}")
}

object CharRecognizer {
  val ALPHABET_CHARACTERS = for (c <- Char.MinValue to Char.MaxValue) yield c

  def apply() = new ScalaCharRecognizer(None)
  def apply(alphabet: Map[String, NeuralNetwork]) = new ScalaCharRecognizer(Some(alphabet))
  def apply(alphabet: java.util.Map[String, NeuralNetwork]) = new JavaCharRecognizer(Some(alphabet))
}

class ScalaCharRecognizer(initAlphabet: Option[Map[String, NeuralNetwork]]) extends CharRecognizer {
  var durationImage: Long = 0L
  var durationExecution: Long = 0L
  var durationBackpropagate: Long = 0L

  override val alphabet: Map[String, NeuralNetwork] = initAlphabet match {
    case None => nonTrainedAlphabet
    case Some(a) => a
  }

  def initWithChars(chars: List[String]): CharRecognizer = CharRecognizer(chars.map(c => c -> initialNeuralNetwork).toMap)

  def learn(image: Image, expected: String): CharRecognizer = {
    def learnInternal = {
      val startTimeImage = System.currentTimeMillis()
      val imageAsVector = new CharImagePreprocessor().prepareImage(image)
      durationImage += (System.currentTimeMillis() - startTimeImage)

      val startTimeExecution = System.currentTimeMillis()
      val neuralExecutions = alphabet.par.mapValues(neural => neural.execute(imageAsVector))
      durationExecution += (System.currentTimeMillis() - startTimeExecution)

      val positive = DenseVector(0.9)
      val negative = DenseVector(0.1)

      val startTimeBackpropagate = System.currentTimeMillis()
      val taughtAlphabet = neuralExecutions.map((e) => e._1 -> e._2.backPropagate(if (e._1 == expected) positive else negative))
      //val taughtAlphabet = neuralExecutions.mapValues(e => (e._1, e._2.backPropagate(if (e._1 == expected) positive else negative)))
      durationBackpropagate += (System.currentTimeMillis() - startTimeBackpropagate)

      val seq: Map[String, NeuralNetwork] = new HashMap[String, NeuralNetwork] ++ taughtAlphabet.seq
      val updatedRecognizer: ScalaCharRecognizer = CharRecognizer(seq)
      updatedRecognizer.durationBackpropagate = durationBackpropagate
      updatedRecognizer.durationExecution = durationExecution
      updatedRecognizer.durationImage = durationImage
      updatedRecognizer
    }

    if (!alphabet.contains(expected)) {
      println("Unknown expected chain \"" + expected + "\" -> creating an initial neural network")
      CharRecognizer(alphabet + (expected -> initialNeuralNetwork)).learn(image, expected)
    } else {
      val updated: ScalaCharRecognizer = learnInternal
      val execution: NeuralNetworkExecution = updated.alphabet(expected).execute(new CharImagePreprocessor().prepareImage(image))
      //println(s"Result on expected: ${expected} => ${execution.output(0)} ::: Recognize: ${recognize(image)}")
      updated
    }
  }

  def recognize(image: Image): Option[String] = {
    val imageAsVector = new CharImagePreprocessor().prepareImage(image)
    val neuralExecutions = alphabet.mapValues(neural => neural.execute(imageAsVector))
    val matches: List[(String, NeuralNetworkExecution)] = neuralExecutions.toList.filter(e => e._2.output(0) > 0).sortBy(e => e._2.output(0)).reverse
    matches match {
      case Nil => None
      case x :: xs => Some(x._1)
    }
  }

  private def nonTrainedAlphabet = new HashMap[String, NeuralNetwork]

  private def initialNeuralNetwork = {
    // 20 x 20 (image size) + 1 (for the additional neuron)
    val A = DenseMatrix.rand[Double](150, 401)
    val B = DenseMatrix.rand[Double](50, 151)
    val C = DenseMatrix.rand[Double](1, 51)
    NeuralNetwork(A, B, C)
  }

  def resetDurations {
    durationImage = 0L
    durationExecution = 0L
    durationBackpropagate = 0L
  }

  def displayDurations = println(s"Duration: image=${durationImage} execution=${durationExecution} back=${durationBackpropagate}")
}
