package net.ocr.image

import com.sksamuel.scrimage.Image
import net.ocr.common.{Rectangle, Block}

/**
 * Extract all the characters from a whole text area
 */
class FullCharExtractor {
  /**
   * Extract all the characters of an image and group them by paragraph.
   *
   * @param image The image to scan for characters
   * @return The list of characters (as Image) grouped by paragraphs.
   */
  def extractAllCharacters(image: Image): List[List[Image]] = {
    val charExtractor = new CharExtractor(image)
    val paragraphs: List[List[Block]] = charExtractor.findParagraphs()
    val words: List[List[Rectangle]] = paragraphs.map(p => charExtractor.findWords(p))
    words.map(p => p.foldRight[List[Image]](Nil)((w, acc) => charExtractor.extractCharacters(w) ::: acc))
  }
}
