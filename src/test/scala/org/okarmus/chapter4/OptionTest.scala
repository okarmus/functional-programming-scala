package org.okarmus.chapter4

import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  test("should return none if any of the elements in none") {
    val list = List(Some(12), None, Some(2),None,None, Some(3))
    val expected = Some(List(12, 2, 3))

    assert(Option.sequence(list) == None)
  }

  test("should properly create optional of list from list of optionals") {
    val list = List(Some(12), Some(2), Some(3))
    val expected = Some(List(12, 2, 3))

    assert(Option.sequence(list) == expected)
  }

  test("traverse should work properly") {
    val list = List("1", "2", "3")
    val expected = Some(List(1, 2, 3))

    assert(
      Option.traverse(list)(x => Some(x.toInt)) == expected
    )
  }

  test("traverse should return None in case of failure") {
    val list = List("1", "2", "dsa", "3")
    assert(
      Option.traverse(list)(x => Option.Try(x.toInt)) == None
    )
  }


}
