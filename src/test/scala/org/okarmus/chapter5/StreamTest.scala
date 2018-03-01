package org.okarmus.chapter5

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  val stream = Stream(1, 2, 3, 4)

  test("should properly return list from a stream") {
    val list = List(1, 2, 3, 4)

    assert(stream.toList == list)
  }

  test("take should return first n elements of the stream") {
    val expected = List(1, 2)

    assert(stream.take(2).toList == expected)

    assert(stream.takeViaUnfold(2).toList == expected)
  }

  test("take should take whole stream is n exceeds stream size") {
    assert(stream.take(10).toList == stream.toList)

    assert(stream.takeViaUnfold(10).toList == stream.toList)
  }

  test("drop should remove first n elements") {
    val expected = List(3, 4)
    assert(stream.drop(2).toList == expected)
  }

  test("drop should remove empty set if n exceeds stream size") {
    assert(stream.drop(10) == Empty)
  }

  test("takeWhile should return elements until match predicate") {
    val expected = List(1, 2)

    assert(
      stream.takeWhile(_ < 3).toList == expected
    )
  }

  test("forAll should indicate if condition is fulfilled") {
    assert(stream.forAll(_ > 0))
    assert(!stream.forAll(_ % 2 == 0))
  }

  test("takeWhileFold should return elements until match predicate") {
    val expected = List(1, 2)

    assert(
      stream.takeWhileFold(_ < 3).toList == expected
    )
  }

  test("map should work properly") {
    val expected = List("1", "2", "3", "4")

    assert(
      stream.map(_.toString).toList == expected
    )
  }

  test("filter should work properly") {
    val expected = List(2, 4)

    assert(stream.filter(_ % 2 == 0).toList == expected)
  }

  test("append should work properly") {
    val expected = List(1, 2, 3, 4, 0, -1)

    assert(
      stream.append(Stream(0, -1)).toList == expected
    )
  }

  test("flatMap should work properly") {
    val streamOfStreams = Stream("This", "is", "sample", "sentence")
    val expected: List[Char] = List('T', 'h', 'i', 's', 'i', 's', 's', 'a', 'm', 'p', 'l', 'e', 's', 'e', 'n', 't', 'e', 'n', 'c', 'e')

    assert(
      streamOfStreams.flatMap(x => Stream(x.toCharArray: _*)).toList == expected
    )
  }

  test("fibs should return fibonacci numbers") {
    val expected = List(1, 1, 2, 3, 5, 8)

    assert(
      Stream.fibs.take(6).toList == expected
    )
  }

  test("fibs using unfold should return fibonacci numbers") {
    val expected = List(1, 1, 2, 3, 5, 8)

    assert(
      Stream.fibsUnfold.take(6).toList == expected
    )
  }

  test("startsWith should properly indicate") {
    val prefix = Stream(1, 2, 3)

    assert(
      stream.startsWith(prefix)
    )

    assert(
      !stream.startsWith(Stream(2, 3))
    )

    assert(
      !stream.startsWith(Stream(1, 2, 3, 4, 5))
    )

  }

  test("tails should work properly") {
    val expected = List(List(1,2,3), List(2,3), List(3), List())

    assert(
      Stream(1, 2, 3).tails.toList.map(_.toList) == expected
    )
  }

  test("hasSubSequence should work properly") {

    assert(
      stream.hasSubSequence(Stream(2, 3))
    )

    assert(
      !stream.hasSubSequence(Stream(2, 4))
    )
  }
}
