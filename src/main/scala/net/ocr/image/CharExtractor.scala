package net.ocr.image

import com.sksamuel.scrimage.Image
import net.ocr.common._
import net.ocr.image.CharExtractor._

/**
 * The aim of the class is to split an image into a bunch of smaller image supposed to represent
 * characters. They are already grouped by words, lines and paragraph
 * Created by cla on 23.08.2014.
 */
class CharExtractor(image: Image) {
  val backgroundColor: Int = image.pixel(0, 0)

  /**
   * Extract all the characters within the given area. The area must represent a line.
   * The characters are searched as followed:
   *
   * <ul>
   *   <li>Search on the diagonal (move (+1, -1)) the first pixel which color is not the background color starting
   *   at the top left corner.</li>
   *   <li>Once found, all the neighbors pixels with a non-background color.</li>
   *   <li>Find the bounds of this list of characters.</li>
   *   <li>Extract as an image the following rectangle (horizontally = bounds, vertically = whole height):
   *     <ul>
   *       <li>left = bounds.x</li>
   *       <li>right = bounds.x + bounds.width</li>
   *       <li>top = 0</li>
   *       <li>bottom = area.height</li>
   *     </ul>
   *   </li>
   * </ul>
   *
   * @param area The area to scan for characters
   * @return The characters as images
   */
  def extractCharacters(area: Rectangle) = {
    // TODO return as rectangle to be able to store/display the characters' bounds?
    val subImage = new SubImage(image, area, backgroundColor)

    def extractCharsInternal(cursor: Int, acc: List[Rectangle]): List[Rectangle] = {
      val firstPixel = subImage.findNextPixel(cursor)

      if (firstPixel.x == -1) acc
      else {
        subImage.extractCharacterFrom(firstPixel) match {
          case Nil => acc
          case charPixels => {
            val charBounds = getBounds(charPixels)
            val completeBounds = Rectangle(area.x + charBounds.x, area.y, charBounds.width, area.height)
            //val charImage = image.subimage(area.x + charBounds.x, area.y + charBounds.y, charBounds.width, charBounds.height)
            val charImage = image.subimage(area.x + charBounds.x, area.y, charBounds.width, area.height)
            //val clearedCharImage = charImage.map((x, y, p) => if (charPixels.contains(XYPosition(x, y))) p else backgroundColor)
            extractCharsInternal(charBounds.x + charBounds.width + 1, completeBounds :: acc)
          }
        }
      }
    }

    extractCharsInternal(0, Nil).reverse
  }

  private def getBounds(positions: List[XYPosition]) = {
    val bounds = positions.foldLeft(Tuple2(XYPosition(Int.MaxValue, Int.MaxValue), XYPosition(0, 0)))((acc, p) => {
      val left = Math.min(acc._1.x, p.x)
      val right = Math.max(acc._2.x, p.x)
      val top = Math.min(acc._1.y, p.y)
      val bottom = Math.max(acc._2.y, p.y)
      Tuple2(XYPosition(left, top), XYPosition(right, bottom))
    })
    Rectangle(bounds._1.x, bounds._1.y, bounds._2.x - bounds._1.x + 1, bounds._2.y - bounds._1.y + 1)
  }

  /**
   * Split the text into paragraphs based on the space between two lines.
   * A small space means "new line in paragraph" when a large space means "new paragraph"
   *
   * @return The list of all the lines grouped by paragraph. A paragraph is a list of lines represented by a Block
   */
  def findParagraphs(): List[List[Block]] = {
    val bits = compressLinesToBits()
    val lineBlocks = bitsToBlock(bits.map(f => f._1))
    val filteredLineBlocks = lineBlocks

    val stats = new Statistics().groupStats(filteredLineBlocks.filter(_.full == false))
    val interlineHeightMax = computeLineHeight(stats).to

    val paragraph = filteredLineBlocks.filter((lineBlock) => lineBlock.full || lineBlock.length > interlineHeightMax).reverse
    paragraph.foldLeft[List[List[Block]]](Nil)((acc, block) => block match {
      case Block(false, _, _) => acc match {
        case Nil => acc
        case x :: xs if x.size == 0 => acc
        case _ => List() :: acc
      }
      case _ => acc match {
        case Nil => List(List(block))
        case x :: xs => (block :: x) :: xs
      }
    }).filter((p) => p.nonEmpty)
  }

  //private def computeLineHeight(blocks: List[Block]) = blocks.foldLeft(0)((acc, block) => acc + (block.length) / blocks.size)
  private def computeLineHeight(stats: List[StatsBlock]) = stats(0)

  /**
   * Split all the lines into a single list of rectangles representing the words. They are recognized based on the
   * space between the characters (same system as the one for the paragraphs). This make sense to give all the lines
   * of a <b>single</b> paragraph to extract all the words that belongs together.
   *
   * @param lines The lines to scan for words
   * @return The list of all the words found for the lines.
   */
  def findWords(lines: List[Block]): List[Rectangle] = {
    lines.foldLeft[List[List[Rectangle]]](Nil)((acc, line) => acc ::: List(findWordsForLine(line))).flatten
  }

  private def findWordsForLine(line: Block): List[Rectangle] = {
    // TODO remove the code duplication with the find paragraphs method
    val bits = compressBlock(line.start, line.end)
    val blocks = bitsToBlock(bits.map(f => f._1))

    val stats = new Statistics().groupStats(blocks.filter(_.full == false))
    val interlineWidthMax = computeLineHeight(stats).to

    val words = blocks.filter((lineBlock) => lineBlock.full || lineBlock.length > interlineWidthMax).reverse
    val wordsElements = words.foldLeft[List[List[Block]]](Nil)((acc, block) => block match {
      case Block(false, _, _) => acc match {
        case Nil => acc
        case x :: xs if x.size == 0 => acc
        case _ => List() :: acc
      }
      case _ => acc match {
        case Nil => List(List(block))
        case x :: xs => (block :: x) :: xs
      }
    }).filter((p) => p.nonEmpty)

    wordsElements
      .map(w => w.tail.foldLeft(Rectangle(w.head.start, line.start, w.head.end - w.head.start, line.end - line.start))
        ((acc, e) => acc.extendsWith(Rectangle(e.start, line.start, e.end - e.start, line.end - line.start))))
  }

  def bitsToBlock(bits: Seq[Boolean]): List[Block] =
    bits.foldLeft[List[Block]](Nil)((acc, bit) => acc match {
      case Nil => Block(bit, 0, 0) :: Nil
      case Block(full, start, end) :: xs =>
        if (full == bit) Block(full, start, end + 1) :: xs
        else Block(bit, end + 1, end + 1) :: acc
    }).reverse

  def compressLinesToBits(): Seq[(Boolean, Int)] = {
    val lines = (for (i <- 0 to image.height - 1) yield compressLine(i)).map(_._2)

    lines.foldLeft[List[(Boolean, Int)]](Nil)((acc, count) => acc match {
      case Nil => List((count > MIN_LINE_NB_PIXELS, count))
      case _ if (count > MIN_LINE_NB_PIXELS) => acc ::: List((true, count))
      case _ if (count < 2) => acc ::: List((false, count))
      case _ => acc ::: List((acc.last._1, count))
    })
  }

  private def compressLine(line: Int): (Boolean, Int) = {
    def compressLineInternal(counter: Int, position: Int): Int = {
      if (position < image.width) compressLineInternal(counter + (if (image.pixel(position, line) != backgroundColor) 1 else 0), position + 1)
      else counter
    }

    val counter = compressLineInternal(0, 0)
    (counter > MIN_LINE_NB_PIXELS, counter)
  }

  private def compressBlock(fromLine: Int, toLine: Int): Seq[(Boolean, Int)] = {
    val columns = (for (i <- 0 to image.width - 1) yield compressColumn(i, fromLine, toLine)).map(_._2)

    columns.foldLeft[List[(Boolean, Int)]](Nil)((acc, count) => acc match {
      case Nil => List((count > MIN_COL_NB_PIXELS, count))
      case _ if (count > MIN_COL_NB_PIXELS) => acc ::: List((true, count))
      case _ if (count < 2) => acc ::: List((false, count))
      case _ => acc ::: List((acc.last._1, count))
    })

//    val convertedLines = for (line <- startLine to endLine) yield convertLineToBooleans(line)
//    val seqWithFalse: Seq[Boolean] = for (e <- 0 to image.width - 1) yield false
//    convertedLines.foldLeft(seqWithFalse)((acc, elem) => acc.zip(elem).map((a) => a._1 || a._2))
  }

  private def compressColumn(column: Int, fromLine: Int, toLine: Int): (Boolean, Int) = {
    def compressColumnInternal(counter: Int, position: Int): Int = {
      if (position <= toLine) compressColumnInternal(counter + (if (image.pixel(column, position) != backgroundColor) 1 else 0), position + 1)
      else counter
    }

    val counter = compressColumnInternal(0, fromLine)
    (counter > MIN_COL_NB_PIXELS, counter)
  }

  private def convertLineToBooleans(line: Int): Seq[Boolean] =
      for (i <- 0 to image.width - 1) yield (image.pixel(i, line) != backgroundColor)
}

object CharExtractor {
  val MIN_LINE_NB_PIXELS = 40
  val MIN_COL_NB_PIXELS = 5
}