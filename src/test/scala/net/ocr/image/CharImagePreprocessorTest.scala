package net.ocr.image

import java.io.File

import com.sksamuel.scrimage.{RGBColor, Color, Image}
import org.scalatest.FunSuite

import scala.io.Source
import scalax.io.Resource

/**
 * Created by cla on 31.07.2014.
 */
class CharImagePreprocessorTest extends FunSuite {
  test("test") {
    val input = new File("src/test/resources/full_test/paragraph_0/char_19.png")
    val image = Image(input)

    for (i <- 0 to image.height - 1) {
      for (j <- 0 to image.width - 1) {
        print(image.pixel(j, i) + " ")
      }
      println()
    }
  }

  test("test color") {
    val color = Color(-5197648)
    println(color);
  }
}
