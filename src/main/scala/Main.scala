import java.io.{FileWriter, FileReader, BufferedReader, File}
import java.nio.file.{Paths, Files}
import java.time.LocalDateTime

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.{Image, Color}
import net.ocr.alphabet.CharRecognizer
import net.ocr.common.{Rectangle, AreaCharacter, AreaWord, AreaParagraph}
import net.ocr.image.{CharImagePreprocessor, FullCharExtractor}

/**
 * Created by cla on 12.06.2014.
 */
object Main {
  def readChars(path: String): List[String] = {
    def consumeReader(reader: BufferedReader, acc: List[String]): List[String] = {
      val line = reader.readLine()
      if (line == null) acc
      else consumeReader(reader, line :: acc)
    }

    val reader: BufferedReader = new BufferedReader(new FileReader(path))
    consumeReader(reader, Nil).reverse
  }

  val INIT_CHARS = readChars("src/main/resources/init-chars.txt")

  def main2(args: Array[String]): Unit = {
    def distToWhite(c: Color): Int = Math.abs(Color.White.getBlue - c.getBlue) + Math.abs(Color.White.getRed - c.getRed) + Math.abs(Color.White.getGreen - c.getGreen)
    def flattenColor(c: Color): Int = if (distToWhite(c) < 250) Color.White.argb else c.argb

    val input = new File("src/main/resources/long-text1.png")
    val raw_image = Image(input)
    val image = raw_image.map((x, y, c) => flattenColor(Color(c)))
    image.write(new File("target/long-text1/filtered.png"))

    val characterAreas: List[AreaCharacter] = new FullCharExtractor().extractAllCharacters(image).head.words.map(w => w.chars).flatten
    val expected: List[String] = readChars("src/main/resources/long-text1-split.txt")
    val images: List[Image] = extractImages(image, characterAreas)

    //saveImages(images)

    println("#Areas: " + characterAreas.length)
    println("#Chars: " + expected.length)

    println(s"First char: ${expected.head}")
    val numberOfLearning: Int = 50
    val recognizer: CharRecognizer = learnNTimes(images.slice(0, 20), expected.slice(0, 20), numberOfLearning)
    println(s"$numberOfLearning learning processes completed")
    val recognized = recognize(recognizer, images.filter(p => p.width > 1 && p.height > 1))
    println(recognized.mkString(""))
  }

  def learnNTimes(images: List[Image], expected: List[String], numberOfLearning: Int = 1): CharRecognizer = {
    def learnInternal(recognizer: CharRecognizer, count: Int): CharRecognizer = count match {
      case _ if (count == numberOfLearning) => recognizer
      case _ => {
        println(s"Start ${count+1}th learning ${LocalDateTime.now()}")
        recognizer.resetDurations
        val taughtRecognizer: CharRecognizer = learn(recognizer, images, expected)
        //taughtRecognizer.displayDurations
        learnInternal(taughtRecognizer, count + 1)
      }
    }

    learnInternal(CharRecognizer().initWithChars(INIT_CHARS), 0)
  }

  def saveImages(images: List[Image]): Unit = {
    for (i <- 0 to images.size - 1) {
      images(i).write(new File(s"target/long-text1/letters/char_$i.png"))
    }
  }

  //def writeRecognizerToFile(fileName: String, ) = Files.write(Paths.get(fileName), )

  def extractImages(image: Image, areas: List[AreaCharacter]): List[Image] =
    areas.map(a => image.subimage(a.bounds.x, a.bounds.y, a.bounds.width, a.bounds.height))

  def learn(charactersAsImage: List[Image], expectedResults: List[String]): CharRecognizer = {
    learn(CharRecognizer().initWithChars(INIT_CHARS), charactersAsImage, expectedResults)
  }

  def learn(recognizer: CharRecognizer, images: List[Image], expected: List[String]): CharRecognizer = expected match {
    case Nil => recognizer
    case "##" :: xs => learn(recognizer, images.tail, expected.tail)
    case x :: xs => learn(recognizer.learn(images.head, expected.head), images.tail, expected.tail)
  }

  def recognize(recognizer: CharRecognizer, charactersAsImage: List[Image]) = {
    def recognizeInternal(notRecognized: List[Image], acc: List[String]): List[String] = notRecognized match {
      case Nil => acc
      case x :: xs =>
        recognizeInternal(xs, acc ::: List(recognizer.recognize(x) match {
          case None => "\n"
          case Some(c) => c
        }))
    }

    recognizeInternal(charactersAsImage, Nil)
  }

//  def recognize(recognizer: CharRecognizer, charactersAsImage: List[List[Image]]) = {
//    def recognizeInternal(remainingParagraphs: List[List[Image]],
//                          currentNotRecognized: List[Image],
//                          accParagraphs: List[List[String]],
//                          currentAcc: List[String]): List[List[String]] = currentNotRecognized match {
//      case Nil => remainingParagraphs match {
//        case Nil => accParagraphs ::: List(currentAcc)
//        case x :: xs => recognizeInternal(xs, x, if (currentAcc.isEmpty) accParagraphs else accParagraphs ::: List(currentAcc), Nil)
//      }
//      case x :: xs if (x.width > 10 || x.height > 10) =>
//        recognizeInternal(remainingParagraphs, xs, accParagraphs, currentAcc ::: List(recognizer.recognize(x) match {
//          case None => "\n"
//          case Some(c) => c
//        }))
//      case x :: xs => recognizeInternal(remainingParagraphs, xs, accParagraphs, currentAcc)
//    }
//
//    recognizeInternal(charactersAsImage, Nil, Nil, Nil)
//  }
}
