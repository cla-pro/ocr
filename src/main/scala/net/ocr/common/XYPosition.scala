package net.ocr.common

/**
 * Created by cla on 25.08.2014.
 */
class XYPosition(_x: Int, _y: Int) {
  def x: Int = _x
  def y: Int = _y

  override def toString = s"XYPosition($x, $y)"
  override def equals(that: Any) =
    that.isInstanceOf[XYPosition] && x == that.asInstanceOf[XYPosition].x && y == that.asInstanceOf[XYPosition].y
}

object XYPosition {
  def apply() = new XYPosition(0, 0)

  def apply(x: Int, y: Int) = new XYPosition(x, y)
}
