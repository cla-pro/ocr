package net.ocr.storage

import org.scalatest.FunSuite

/**
 * Created by cla on 19.12.2014.
 */
class JsonObjectTest extends FunSuite {
  test("Test the json formatted serialization (*)") {
    val jsonObject: JsonObject = JsonObject("a" -> "b", "c" -> "asdf", ("d" -> JsonObject("e" -> "aaélsjf", "f" -> "g")))
    assert("{\n  a: b,\n  c: asdf,\n  d: {\n    e: aaélsjf,\n    f: g\n  }\n}" === jsonObject.serialize)
  }

  test("Test the json formatted serialization (List)") {
    val jsonObject: JsonObject = JsonObject(List("a" -> "b", "c" -> "asdf", ("d" -> JsonObject(List("e" -> "aaélsjf", "f" -> "g")))))
    assert("{\n  a: b,\n  c: asdf,\n  d: {\n    e: aaélsjf,\n    f: g\n  }\n}" === jsonObject.serialize)
  }

  test("Test equals") {
    val jsonObject1: JsonObject = JsonObject(List("a" -> "b", "d" -> JsonObject(List(("e" -> "f")))))
    val jsonObject2: JsonObject = JsonObject(List("a" -> "b"))
    val jsonObject3: JsonObject = JsonObject(List("a" -> "c", "d" -> JsonObject(List(("e" -> "f")))))
    val jsonObject4: JsonObject = JsonObject(List("a" -> "b", "d" -> JsonObject(List(("e" -> "f")))))

    assert(jsonObject1.equals(null) == false)
    assert(jsonObject1.equals(Nil) == false)
    assert(jsonObject1.equals(jsonObject2) == false)
    assert(jsonObject1.equals(jsonObject3) == false)
    assert(jsonObject1.equals(jsonObject4) == true)
  }
}
