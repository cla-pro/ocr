package net.ocr.image

import java.io.File

import com.sksamuel.scrimage.{Image, Color}
import net.ocr.common.{Rectangle, AreaCharacter, AreaWord, AreaParagraph}
import org.scalatest.FunSuite

class FullCharExtractorTest extends FunSuite {
  test("Full Char Extractor") {
    def distToWhite(c: Color): Int = Math.abs(Color.White.getBlue - c.getBlue) + Math.abs(Color.White.getRed - c.getRed) + Math.abs(Color.White.getGreen - c.getGreen)
    def flattenColor(c: Color): Int = if (distToWhite(c) < 250) Color.White.argb else c.argb

    val input = new File("src/test/resources/long-text1.png")
    val raw_image = Image(input)
    val image = raw_image.map((x, y, c) => flattenColor(Color(c)))
    image.write(new File("src/test/resources/long-text1/base.png"))

    val fullCharExtractor = new FullCharExtractor
    val paragraphs = fullCharExtractor.extractAllCharacters(image)

    for (j <- 0 to paragraphs.size - 1) {
      val words: List[AreaWord] = paragraphs(j).words
      for (i <- 0 to words.size - 1) {
        val characters: List[AreaCharacter] = words(i).chars
        for (k <- 0 to characters.size - 1) {
          val c: Rectangle = characters(k).bounds
          image.subimage(c.x, c.y, c.width, c.height)
            .write(new File(s"src/test/resources/long-text1/paragraph_$j/char_$i.png"))
        }
      }
    }
  }
}
