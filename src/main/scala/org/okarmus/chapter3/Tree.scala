package org.okarmus.chapter3

trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  //Ex. 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  //Ex. 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  //Ex. 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left) max depth(right)
  }

  //Ex. 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  //Ex. 3.29
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _)

  def maximumFold(tree: Tree[Int]): Int = fold(tree)(x => x)(_ max _)

  def depthFold[A](tree: Tree[A]): Int = fold(tree)(x => 0)((d1, d2) => 1 + (d1 max d2))

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}