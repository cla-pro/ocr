package net.ocr.common

import org.scalatest.FunSuite

/**
 * Created by cla on 30.12.2014.
 */
class RectangleTest extends FunSuite {
  test("Test the rectangle extension") {
    // Overlapping
    assertResult(Rectangle(0, 0, 6, 6))(Rectangle(0, 0, 4, 4).extendsWith(Rectangle(2, 2, 4, 4)))
    // 1 include 2
    assertResult(Rectangle(0, 0, 4, 4))(Rectangle(0, 0, 4, 4).extendsWith(Rectangle(1, 1, 2, 2)))
    // 2 include 1
    assertResult(Rectangle(0, 0, 4, 4))(Rectangle(1, 1, 2, 2).extendsWith(Rectangle(0, 0, 4, 4)))
    // Disjoint
    assertResult(Rectangle(0, 0, 4, 4))(Rectangle(0, 0, 1, 1).extendsWith(Rectangle(3, 3, 1, 1)))
  }
}
