package net.ocr.image

import com.sksamuel.scrimage.Image
import net.ocr.common._

/**
 * Extract all the characters from a whole text area
 */
class FullCharExtractor {
  /**
   * Extract all the characters of an image and group them by paragraph and words.
   * Their coordinates in the image are returned for the caller to decide how to handle
   * these information (show on a GUI or extract the images).
   *
   * @param image The image to scan for characters
   * @return The list of characters (as Rectangles) grouped by paragraphs and words.
   */
  def extractAllCharacters(image: Image): List[AreaParagraph] = {
    val charExtractor = new CharExtractor(image)
    val paragraphs: List[List[Block]] = charExtractor.findParagraphs().slice(0, 1)
    val words: List[List[Rectangle]] = paragraphs.map(p => charExtractor.findWords(p))
    words.map(p => AreaParagraph(p.map(w => AreaWord(charExtractor.extractCharacters(w).map(c => AreaCharacter(c))))))
    //words.map(p => p.foldRight[List[Image]](Nil)((w, acc) => charExtractor.extractCharacters(w) ::: acc))
  }
}
