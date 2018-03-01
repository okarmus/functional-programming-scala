package org.okarmus.chapter2

import org.scalatest.FunSuite
import Chapter2._

class Chapter2Test extends FunSuite {

  test("fib should throw exception when n is lower than 1") {
    assertThrows[IllegalArgumentException](
      fib(0)
    )
  }

  test("fib should return proper fibonacci numbers") {
    val cases: List[(Int, Int)] = List((1, 0), (2, 1), (3, 1), (4, 2), (5, 3), (6, 5))
    cases.foreach { case (n, expected) => assert(fib(n) == expected, s"${n}th number of fibonacci should be $expected") }
  }

  test("isSorted should properly indicate") {
    val ordering: (Int, Int) => Boolean = (a, b) => a < b

    assert(isSorted(Array(1, 4, 6, 7, 8), ordering), "The array is sorted")

    assert(!isSorted(Array(1, 4, 2, 7, 8), ordering), "The array is not sorted")
  }



}
