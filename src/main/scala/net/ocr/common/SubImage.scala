package net.ocr.common

import com.sksamuel.scrimage.Image

import scala.collection.immutable.Queue

/**
 * Created by cla on 02.09.2014.
 */
class SubImage(image: Image, area: Rectangle, backgroundColor: Int) {
  def pixel(x: Int, y: Int): Int =
    if (isValidPosition(x, y)) image.pixel(area.x + x, area.y + y)
    else throw new ArrayIndexOutOfBoundsException

  def findNextPixel(fromX: Int): XYPosition = {
    for (x <- fromX to area.width) {
      for (y <- 0 to area.height) {
        if (pixel(x, y) != backgroundColor) {
          return XYPosition(x, y)
        }
      }
    }
    XYPosition(-1, -1)
  }

  def extractCharacterFrom(reference: XYPosition): List[XYPosition] = {
    def extractChars(currentPixel: XYPosition, found: List[XYPosition], toCheck: Queue[XYPosition]): List[XYPosition] = {
      val neighbors = List(
        XYPosition(currentPixel.x - 1, currentPixel.y),
        XYPosition(currentPixel.x, currentPixel.y - 1),
        XYPosition(currentPixel.x + 1, currentPixel.y),
        XYPosition(currentPixel.x, currentPixel.y + 1))

      val filteredNeighbors = neighbors.filter((p) => isValidPosition(p)).filter((p) => pixel(p.x, p.y) != backgroundColor)
        .filter((p) => !found.contains(p) && !toCheck.contains(p))

      val newToCheck = toCheck ++ filteredNeighbors
      if (newToCheck.isEmpty) found
      else extractChars(newToCheck.head, currentPixel :: found, newToCheck.tail)
    }

    extractChars(reference, Nil, Queue[XYPosition]())
  }

  def toImage: Image = image.subimage(area.x, area.y, area.width, area.height)

  private def isValidPosition(p: XYPosition): Boolean = isValidPosition(p.x, p.y)
  private def isValidPosition(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x <= area.x + area.width && y <= area.y + area.height
}
