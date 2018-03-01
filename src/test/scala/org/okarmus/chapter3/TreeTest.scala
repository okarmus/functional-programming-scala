package org.okarmus.chapter3

import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("should properly return size") {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

    assert(Tree.size(tree) == 5)

  }

  test("should properly return maximum") {
    val tree = Branch(Leaf(1), Branch(Leaf(5), Leaf(3)))

    assert(Tree.maximum(tree) == 5)
  }

  test("map should work properly") {
    val tree = Branch(Leaf(1), Branch(Leaf(5), Leaf(3)))
    val expected = Branch(Leaf("1"), Branch(Leaf("5"), Leaf("3")))

    assert(Tree.map(tree)(_.toString) == expected)
  }

}
