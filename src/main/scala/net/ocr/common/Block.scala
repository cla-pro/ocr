package net.ocr.common

/**
 * Created by cla on 31.08.2014.
 */
case class Block(full: Boolean, start: Int, end: Int) {
  def length: Int = end - start + 1
}
