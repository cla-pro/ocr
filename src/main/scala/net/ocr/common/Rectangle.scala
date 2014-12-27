package net.ocr.common

/**
 * Created by cla on 26.08.2014.
 */
class Rectangle(_x: Int, _y: Int, _width: Int, _height: Int) {
  val x = _x
  val y = _y
  val width = _width
  val height = _height

  override def toString = s"Rectangle($x, $y, $width, $height)"
}

object Rectangle {
  def apply() = new Rectangle(0, 0, 0, 0)
  def apply(x: Int, y: Int, width: Int, height: Int) = new Rectangle(x, y, width, height)
}
