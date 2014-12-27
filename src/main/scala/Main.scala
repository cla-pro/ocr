import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.{Image, Color}
import net.ocr.alphabet.CharRecognizer
import net.ocr.image.FullCharExtractor

/**
 * Created by cla on 12.06.2014.
 */
object Main {
  def main(args: Array[String]): Unit = {
    def distToWhite(c: Color): Int = Math.abs(Color.White.getBlue - c.getBlue) + Math.abs(Color.White.getRed - c.getRed) + Math.abs(Color.White.getGreen - c.getGreen)
    def flattenColor(c: Color): Int = if (distToWhite(c) < 250) Color.White.argb else c.argb

    val input = new File("src/test/resources/long-text1.png")
    val raw_image = Image(input)
    val image = raw_image.map((x, y, c) => flattenColor(Color(c)))

    val charactersAsImage: List[List[Image]] = new FullCharExtractor().extractAllCharacters(image)
    val recognized = recognize(charactersAsImage)
    println(recognized.map(p => p.mkString(" ")).mkString("\n\n"))
  }

  def recognize(charactersAsImage: List[List[Image]]) = {
    val recognizer: CharRecognizer = CharRecognizer()
    def recognizeInternal(remainingParagraphs: List[List[Image]],
                          currentNotRecognized: List[Image],
                          accParagraphs: List[List[Char]],
                          currentAcc: List[Char]): List[List[Char]] = currentNotRecognized match {
      case Nil => remainingParagraphs match {
        case Nil => accParagraphs ::: List(currentAcc)
        case x :: xs => recognizeInternal(xs, x, if (currentAcc.isEmpty) accParagraphs else accParagraphs ::: List(currentAcc), Nil)
      }
      case x :: xs if (x.width > 10 || x.height > 10) => recognizeInternal(remainingParagraphs, xs, accParagraphs, currentAcc ::: List(recognizer.recognize(x)))
      case x :: xs => recognizeInternal(remainingParagraphs, xs, accParagraphs, currentAcc)
    }

    recognizeInternal(charactersAsImage, Nil, Nil, Nil)
  }
}
