import java.io.{FileReader, BufferedReader, File}

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.{Image, Color}
import net.ocr.alphabet.CharRecognizer
import net.ocr.common.{Rectangle, AreaCharacter, AreaWord, AreaParagraph}
import net.ocr.image.FullCharExtractor

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

  def main(args: Array[String]): Unit = {
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

    val recognizer: CharRecognizer = learn(List(images.head), List(expected.head))
    //val recognized = recognize(recognizer, images.filter(p => p.width > 1 && p.height > 1))
    //println(recognized.mkString(""))
  }

  def saveImages(images: List[Image]): Unit = {
    for (i <- 0 to images.size - 1) {
      images(i).write(new File(s"target/long-text1/letters/char_$i.png"))
    }
  }

  def extractImages(image: Image, areas: List[AreaCharacter]): List[Image] =
    areas.map(a => image.subimage(a.bounds.x, a.bounds.y, a.bounds.width, a.bounds.height))

  def learn(charactersAsImage: List[Image], expectedResults: List[String]): CharRecognizer = {
    def learnInternal(recognizer: CharRecognizer, images: List[Image], expected: List[String]): CharRecognizer = expected match {
      case Nil => recognizer
      case "##" :: xs => {
        println("Ignoring ##")
        learnInternal(recognizer, images.tail, expected.tail)
      }
      case x :: xs => {
        println("Learning with expected chars: " + expected.head)
        learnInternal(recognizer.learn(images.head, expected.head), images.tail, expected.tail)
      }
    }

    learnInternal(CharRecognizer().initWithChars(INIT_CHARS), charactersAsImage, expectedResults)
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
