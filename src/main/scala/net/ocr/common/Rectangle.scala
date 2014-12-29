package net.ocr.common

import scala.math._

/**
 * Created by cla on 26.08.2014.
 */
class Rectangle(_x: Int, _y: Int, _width: Int, _height: Int) {
  val x = _x
  val y = _y
  val width = _width
  val height = _height

  def extendsWith(other: Rectangle): Rectangle = {
    def right = x + width
    def top = y + height
    def otherRight = other.x + other.width
    def otherTop = other.y + other.height

    def newLeft = min(x, other.x)
    def newRight = min(right, otherRight)
    def newBottom = min(y, other.y)
    def newTop = min(top, otherTop)
    Rectangle(newLeft, newBottom, newRight - newLeft, newTop - newBottom)
  }

  override def toString = s"Rectangle($x, $y, $width, $height)"
}

object Rectangle {
  def apply() = new Rectangle(0, 0, 0, 0)
  def apply(x: Int, y: Int, width: Int, height: Int) = new Rectangle(x, y, width, height)
}
