package org.okarmus.chapter3

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("should return tail of not empty list") {
    val list = List(1, 2, 3)
    assert(List.tail(list) == List(2, 3))
  }

  test("should throw exception when trying to get tail on empty list") {
    assertThrows[UnsupportedOperationException](List.tail(Nil))
  }

  test("should replace head of not empty List") {
    val list = List(3, 4, 5, 6)
    val newHead = 0
    val expected = List(newHead, 4, 5, 6)

    assert(List.setHead(list, newHead) == expected)
  }

  test("should throw exception when trying to replace head on empty list") {
    assertThrows[UnsupportedOperationException](List.setHead(Nil, 3))
  }

  test("should drop first n elements of the list") {
    val list = List(1, 2, 3, 4, 5)
    val n = 3
    val expected = List(4, 5)
    assert(List.drop(list, n) == expected)

    assert(List.drop(list, 5) == Nil)
  }

  test("should throw exception when n is bigger than list size") {
    val list = List(1, 2, 3)
    val n = 4

    assertThrows[UnsupportedOperationException](List.drop(list, n))
  }

  test("drop while should return empty list if all matches predicate") {
    val list = List(1, 2, 3)
    assert(List.dropWhile(list)(x => x > 0) == Nil)
  }

  test("drop while should properly drop elements") {
    val list = List(1, 2, 3, 2, 1)
    val predicate: Int => Boolean = x => x < 3
    val expected = List(3, 2, 1)

    assert(List.dropWhile(list)(predicate) == expected)
  }

  test("init should return everything except last element") {
    val list = List(1, 2, 3, 4)
    val expected = List(1, 2, 3)
    assert(List.init(list) == expected)

    assertThrows[UnsupportedOperationException](List.init(Nil))
  }

  test("should return identical list") {
    val list = List(1, 2, 3)

    assert(List.identical(list) == list)
  }

  test("should properly return the length of the list") {
    val list = List(1, 2, 3, 4)

    assert(List.length(list) == 4)

    assert(List.length(Nil) == 0)
  }

  test("sum left should work properly") {
    val list = List(1, 2, 3)
    assert(List.sumLeft(list) == 6)
  }

  test("product left should work properly") {
    val list: List[Double] = List(1, 2, 3)

    assert(List.productLeft(list) == 6)
  }

  test("should properly reverse list") {
    val list = List(1, 2, 3)
    val expected = List(3, 2, 1)

    assert(List.reverse(list) == expected)
  }

  test("list append using fold should work properly") {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5)
    val expected = List(1, 2, 3, 4, 5)

    assert(List.appendFold(l1, l2) == expected)

    assert(List.appendFold(l1, Nil) == l1)

    assert(List.append(Nil, l2) == l2)
  }

  test("concat should work properly") {
    val lists = List(List(1, 2), Nil, List(3, 4), List(1))
    val expected = List(1, 2, 3, 4, 1)

    assert(List.concat(lists) == expected)
  }

  test("add1 should properly increment all elements in list") {
    val list = List(1, 2, 3)
    val expected = List(2, 3, 4)

    assert(List.add1(list) == expected)
  }

  test("should properly create string from list") {
    val list: List[Double] = List(1, 2, 4.5, 5)
    val expected: List[String] = List("1.0", "2.0", "4.5", "5.0")

    assert(List.listToString(list) == expected)
  }

  test("map should work properly") {
    val list = List(0, 1, 2, 3)
    val expected = List(0, 2, 4, 6)

    assert(List.map(list)(2 * _) == expected)
  }

  test("filter should work properly") {
    val list = List(0, 1, 2, 3)
    val expected = List(0, 2)

    assert(List.filter(list)(_ % 2 == 0) == expected)
  }

  test("flatMap should work properly") {
    val list = List("This", "is", "sample", "sentence")
    val expected: List[Char] = List('T', 'h', 'i', 's', 'i', 's', 's', 'a', 'm', 'p', 'l', 'e', 's', 'e', 'n', 't', 'e', 'n', 'c', 'e')

    assert(
      List.flatMap(list)(x => List(x.toCharArray: _*)) == expected
    )
  }

  test("filter using map should work properly") {
    val list = List(0, 1, 2, 3)
    val expected = List(0, 2)

    assert(List.filterUsingMap(list)(_ % 2 == 0) == expected)
  }

  test("add pairwise should work properly") {
    val l1 = List(1, 2, 3, 4)
    val l2 = List(5, 6, 7, 8)

    val expected = List(6, 8, 10, 12)

    assert(List.addPairwise(l1, l2) == expected)
  }

  test("add pairwise should work until any of the list is empty") {
    val l1 = List(1, 2, 4)
    val l2 = List(1, 1)

    val expected = List(2, 3)

    assert(List.addPairwise(l1, l2) == expected)
    assert(List.addPairwise(l2, l1) == expected)
  }

  test("zip with should work properly") {
    val as = List(1, 2, 3)
    val bs = List(1.2, 2.3, 3.1)

    val expected = List("1: 1.2", "2: 2.3", "3: 3.1")

    assert(
      List.zipWith(as, bs)((a, b) => s"$a: $b") == expected
    )
  }

  test("should properly indicate is subsequence exists") {
    val sup = List(1, 2, 3, 4)

    assert(List.hasSubSequence(sup, List(1, 2)))
    assert(List.hasSubSequence(sup, List(2, 3)))
    assert(List.hasSubSequence(sup, List(4)))
    assert(List.hasSubSequence(sup, List(1, 2, 3, 4)))
    assert(List.hasSubSequence(List(1, 2, 1, 2, 3, 4), List(1, 2, 3, 4)))
    assert(List.hasSubSequence(sup, Nil))


    assert(!List.hasSubSequence(List(1, 2, 4, 2, 3, 4), List(1, 2, 3, 4)))
    assert(!List.hasSubSequence(sup, List(1, 2, 4)))
  }

}
