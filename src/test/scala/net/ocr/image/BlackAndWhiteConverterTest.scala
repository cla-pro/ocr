package net.ocr.image

import java.io.File

import com.sksamuel.scrimage.filter.GrayscaleFilter
import com.sksamuel.scrimage.{PixelTools, Color, Image}
import org.scalatest.FunSuite

/**
 * Created by cla on 25.08.2014.
 */
class BlackAndWhiteConverterTest extends FunSuite {
  test("Convert to B/W") {
    Console.println("Black = " + Color.Black)
    Console.println("White = " + Color.White)
    val input = new File("src/test/resources/text_2.png")
    val image = Image(input)
    //val bwImage = new BlackAndWhiteConverter().convert(image)
    //val bwImage = image.filter(GrayscaleFilter)
    val bwImage = image.map((x, y, p) => {
      val red = 0.33 * PixelTools.red(p)
      val green = 0.33 * PixelTools.green(p)
      val blue = 0.33 * PixelTools.blue(p)
      val gray = red + green + blue
      PixelTools.rgb(gray.toInt, gray.toInt, gray.toInt)
    })

    val output = new File("src/test/resources/bw.png")
    bwImage.write(output)
  }
}
