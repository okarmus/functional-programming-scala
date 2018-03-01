package org.okarmus.chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum(xs: List[Int]): Int = foldRight(xs, 0)(_ + _)

  def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  //Ex. 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new UnsupportedOperationException("Nil.tail")
    case Cons(_, tail) => tail
  }

  //Ex. 3.3
  def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
    case Nil => throw new UnsupportedOperationException("Nil.setHead")
    case Cons(_, tail) => Cons(newHead, tail)
  }

  //Ex 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, x) if x > 0 => throw new UnsupportedOperationException("Nil.drop")
    case (_, 0) => l
    case (Cons(_, tail), _) => drop(tail, n - 1)
  }

  //Ex 3.5
  def dropWhile[A](l: List[A])(f: (A) => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail)(f) else l
  }

  //Ex. 3.7
  def identical[A](as: List[A]) = foldRight(as, Nil: List[A])(Cons(_, _))

  //Ex. 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  //Ex. 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //Ex. 3.11
  def sumLeft(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  //Ex. 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((reversed, a) => Cons(a, reversed))

  //Ex 3.13
  def foldRightUsingLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((a, b) => f(b, a))

  //Ex 3.14
  def appendFold[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys)((x, res) => Cons(x, res))

  //Ex. 3.15
  def concat[A](xss: List[List[A]]): List[A] = foldRight(xss, Nil: List[A])(append)


  //Ex. 3.16
  def add1(xs: List[Int]): List[Int] = foldRight(xs, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  //Ex 3.17
  def listToString(xs: List[Double]): List[String] = foldRight(xs, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  //Ex 3.18
  def map[A, B](xs: List[A])(f: A => B): List[B] = {
    @tailrec
    def inner(toCheck: List[A], result: List[B]): List[B] = toCheck match {
      case Nil => result
      case Cons(h, t) => inner(t, Cons(f(h), result))
    }

    inner(reverse(xs), Nil)
  }

  //Ex 3.19
  def filter[A](xs: List[A])(f: A => Boolean): List[A] = foldRightUsingLeft(xs, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  //Ex 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(List.map(as)(f))

  //foldRightUsingLeft(as, Nil: List[B])((a, acc) => append(f(a), acc))

  //Ex. 3.21
  def filterUsingMap[A](as: List[A])(f: A => Boolean) = flatMap(as)(a => if (f(a)) List(a) else Nil)

  //Ex. 3.22
  def addPairwise(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  //Ex 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {

    @tailrec
    def inner(toCheckA: List[A], toCheckB: List[B], acc: List[C]): List[C] = (toCheckA, toCheckB) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(a, tailA), Cons(b, tailB)) => inner(tailA, tailB, Cons(f(a, b), acc))
    }

    reverse(inner(as, bs, Nil))
  }

  //Ex 3.24
  def startsWith[A](as: List[A], prefix: List[A]): Boolean = (as, prefix) match {
    case (_, Nil) => true
    case (Cons(h,t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubSequence(t, sub)
  }

  /*def hasSubSequence[A](sup: List[A], primalSub: List[A]): Boolean = {
    @tailrec
    def inner(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(supHead, supTail),Cons(subHead, subTail)) if sub == primalSub => if (supHead == subHead) inner(supTail, subTail) else inner(supTail, primalSub)
      case (Cons(supHead, supTail),Cons(subHead, subTail)) => if (supHead == subHead) inner(supTail, subTail) else inner(sup, primalSub)
     }
    inner(sup, primalSub)
  }*/

  def productLeft(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("Nil.init")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

}