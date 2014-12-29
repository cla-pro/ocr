import java.io.{FileReader, BufferedReader, File}

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.{Image, Color}
import net.ocr.alphabet.CharRecognizer
import net.ocr.common.{AreaCharacter, AreaWord, AreaParagraph}
import net.ocr.image.FullCharExtractor

/**
 * Created by cla on 12.06.2014.
 */
object Main {

  def readChars(path: String): List[Char] = {
    def consumeReader(reader: BufferedReader, text: String): String = {
      val line = reader.readLine()
      if (line == null) text
      else consumeReader(reader, text + line)
    }

    val reader: BufferedReader = new BufferedReader(new FileReader(path))
    consumeReader(reader, "").toCharArray.toList
  }

  def main(args: Array[String]): Unit = {
    def distToWhite(c: Color): Int = Math.abs(Color.White.getBlue - c.getBlue) + Math.abs(Color.White.getRed - c.getRed) + Math.abs(Color.White.getGreen - c.getGreen)
    def flattenColor(c: Color): Int = if (distToWhite(c) < 250) Color.White.argb else c.argb

    val input = new File("src/main/resources/long-text1.png")
    val raw_image = Image(input)
    val image = raw_image.map((x, y, c) => flattenColor(Color(c)))
    image.write(new File("target/long-text1/filtered.png"))

    val charactersAsImage: List[AreaCharacter] = new FullCharExtractor().extractAllCharacters(image).head.words.map(w => w.chars).flatten
    val expectedChars: List[Char] = readChars("src/main/resources/long-text1.txt")

    //saveImages(charactersAsImage)

    println("#Areas: " + charactersAsImage.length)
    println("#Chars: " + expectedChars.length)

    //val recognized = recognize(charactersAsImage)
    //println(recognized.map(p => p.mkString(" ")).mkString("\n\n"))
  }

  def saveImages(images: List[Image]): Unit = {
    for (i <- 0 to images.size - 1) {
      images(i).write(new File(s"target/long-text1/char_$i.png"))
    }
  }

  //def learn(charactersAsImage: List[Image])

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
      case x :: xs if (x.width > 10 || x.height > 10) =>
        recognizeInternal(remainingParagraphs, xs, accParagraphs, currentAcc ::: List(recognizer.recognize(x) match {
          case None => '\n'
          case Some(c) => c
        }))
      case x :: xs => recognizeInternal(remainingParagraphs, xs, accParagraphs, currentAcc)
    }

    recognizeInternal(charactersAsImage, Nil, Nil, Nil)
  }
}
