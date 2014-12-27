package net.ocr.image

import java.io.File

import com.sksamuel.scrimage.{Color, Image}
import net.ocr.common.{Rectangle, Block}
import org.scalatest.FunSuite

class CharExtractorTest extends FunSuite {
  test("Full Test") {
    def distToWhite(c: Color): Int = Math.abs(Color.White.getBlue - c.getBlue) + Math.abs(Color.White.getRed - c.getRed) + Math.abs(Color.White.getGreen - c.getGreen)
    def flattenColor(c: Color): Int = if (distToWhite(c) < 250) Color.White.argb else c.argb

    val input = new File("src/test/resources/grayscale.png")
    val raw_image = Image(input)
    val image = raw_image.map((x, y, c) => flattenColor(Color(c)))
    val charExtractor = new CharExtractor(image)
    val paragraphs = charExtractor.findParagraphs()

    for (j <- 0 to paragraphs.size - 1) {
      val words = charExtractor.findWords(paragraphs(j))
      val nbWords = words.length
      val characters = (for (w <- words) yield charExtractor.extractCharacters(w).reverse).flatten

      for (i <- 0 to characters.size - 1) {
        characters(i).write(new File(s"src/test/resources/full_test/paragraph_$j/char_$i.png"))
      }
    }
  }

  test("First test") {
    val input = new File("src/test/resources/grayscale.png")
    val image = Image(input)
    val characters = new CharExtractor(image).extractCharacters(Rectangle(9, 15, 92, 50))

//    for (i <- 0 to characters.size - 1)
//      characters(i).write(new File(s"src/test/resources/first_test/char_$i.png"))
  }

  test("Paragraph extractor") {
    val input = new File("src/test/resources/grayscale.png")
    val image = Image(input)
    val paragraphs = new CharExtractor(image).findParagraphs()
    Console.println(paragraphs)
  }

  test("Test Paragraph extractor") {
    val input = new File("src/test/resources/grayscale_2.png")
    val image = Image(input)
    val paragraphs = new CharExtractor(image).findParagraphs()
    assert(paragraphs === List(List(Block(true, 4, 13), Block(true, 17, 26)),
      List(Block(true, 35, 44), Block(true, 47, 56)),
      List(Block(true, 66, 75), Block(true, 78, 87), Block(true, 91, 100))))
  }

  test("Paragraph test") {
    val input = new File("src/test/resources/grayscale.png")
    val image = Image(input)
    val paragraphs = new CharExtractor(image).findParagraphs()
    Console.println(paragraphs)
  }

  test("Words extractor") {
    val input = new File("src/test/resources/grayscale.png")
    val image = Image(input)
    val words = new CharExtractor(image).findWords(List(Block(true, 15, 64), Block(true, 78, 127)))
    Console.println(words)
  }

  test("Test the line to bits") {
    val input = new File("src/test/resources/grayscale.png")
    val image = Image(input)
    val bits = new CharExtractor(image).compressLinesToBits().toList
    for (i <- 0 to bits.size - 1) {
      Console.println("Line: " + i + " => " + bits(i))
    }
  }
}
