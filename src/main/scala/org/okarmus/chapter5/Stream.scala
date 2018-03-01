package org.okarmus.chapter5

import scala.annotation.tailrec
import Stream._

trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  //Ex. 5.1
  def toList: List[A] = {
    @tailrec
    def inner(stream: Stream[A], result: List[A]): List[A] = stream match {
      case Empty => result
      case Cons(h, t) => inner(t(), h() :: result)
    }

    inner(this, Nil).reverse
  }

  //Ex. 5.2
  def take(n: Int): Stream[A] = (this, n) match {
    case (Empty, _) => Empty
    case (_, 0) => Empty
    case (Cons(h, t), x) => cons(h(), t().take(n - 1))
  }

  //Ex. 5.3
  @tailrec
  final def drop(n: Int): Stream[A] = (this, n) match {
    case (Empty, _) => Empty
    case (_, 0) => this
    case (Cons(_, t), x) => t().drop(x - 1)
  }

  //Ex. 5.4
  def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

  //Ex. 5.5
  def takeWhileFold(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if  (f(a)) cons(a, b)
      else empty
    )

  //Ex. 5.6
  def headOptionFold: Option[A] = foldRight(None: Option[A])((a , _) => Some(a))

  //Ex. 5.7
  def map[B](f: A => B): Stream[B] = this.foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = this.foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def withFilter(f: A => Boolean): Stream[A] = filter(f)

  def append[B >: A](b: => Stream[B]): Stream[B] = this.foldRight(b)((a, acc) => cons(a, acc) )

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B])((a, acc) => f(a) append acc)


  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case Empty => false
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

}

object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  //Ex. 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  //Ex. 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //Ex. 5.10
  def fibs: Stream[Int] = {
    def inner(n_2: Int, n_1: Int): Stream[Int] = {
      lazy val n = n_1 + n_2
      cons(n, inner(n_1, n))
    }
    cons(1, cons(1, inner(1, 1)))
  }

  //Ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }


  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}


object Main extends App {
  println(
    Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList
  )
}