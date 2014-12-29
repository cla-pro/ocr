package net.ocr.common

/**
 * Created by cla on 29.12.2014.
 */
abstract class Area {
  def bounds: Rectangle
}

case class AreaParagraph(words: List[AreaWord]) extends Area {
  override def bounds: Rectangle = words.map(w => w.bounds).tail
    .foldLeft(words.head.bounds)((acc, b) => acc.extendsWith(b))
}

case class AreaWord(chars: List[AreaCharacter]) extends Area {
  override def bounds: Rectangle = chars.map(w => w.bounds).tail
    .foldLeft(chars.head.bounds)((acc, b) => acc.extendsWith(b))
}

case class AreaCharacter(bounds: Rectangle) extends Area
