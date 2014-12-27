package net.ocr.storage

/**
 * Created by cla on 19.12.2014.
 */
class JsonObject(c: Map[String, Any]) {
  private val _content = c;
  private val TAB_SIZE = 2

  def content: Map[String, Any] = _content
  def apply(key: String) = _content(key)
  def serialize: String = serializeFormatted(0)

  private def serializeFormatted(tab: Int): String =
    _content.map(e => createTabString(tab + TAB_SIZE) + e._1 + ": " + (
      if (e._2.isInstanceOf[JsonObject]) e._2.asInstanceOf[JsonObject].serializeFormatted(tab + TAB_SIZE)
      else e._2.toString)).mkString("{\n", ",\n", "\n" + createTabString(tab) + "}")

  private def createTabString(length: Int): String =
    if (length > 0) (for (i <- 0 to length - 1) yield " ").mkString("", "", "")
    else ""

  override def equals(that: Any) =
    if (that == null) false
    else if (that.isInstanceOf[JsonObject]) {
      val other: JsonObject = that.asInstanceOf[JsonObject]
      _content.equals(other._content)
    }
    else false

  override def toString: String = serialize
}

object JsonObject {
  def apply[T <: Any](content: (String, T)*) = new JsonObject(content.toMap)
  def apply[T <: Any](content: List[(String, T)]) = new JsonObject(content.toMap)
}
