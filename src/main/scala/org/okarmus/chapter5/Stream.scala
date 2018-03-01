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
      if (f(a)) cons(a, b)
      else empty
    )

  //Ex. 5.6
  def headOptionFold: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  //Ex. 5.7
  def map[B](f: A => B): Stream[B] = this.foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = this.foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def withFilter(f: A => Boolean): Stream[A] = filter(f)

  def append[B >: A](b: => Stream[B]): Stream[B] = this.foldRight(b)((a, acc) => cons(a, acc))

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

  //Ex 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case _ => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Empty, _) => None
    case (_, 0) => None
    case (Cons(h, t), x) => Some((h(), (t(), x - 1)))
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }

  //Ex. 5.14
  def startsWith[AA >: A](prefix: Stream[AA]): Boolean =
    this
      .zipAll(prefix)
      .takeWhile(_._2.isDefined)
      .forAll { case (h1, h2) => h1 == h2 }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(Cons(h, t), t())
  } append Stream(empty)

  def hasSubSequence[A](sub: Stream[A]): Boolean = tails exists (_ startsWith sub)

  /*    this match {
        case Empty => false
        case Cons(h, t) => if (this startsWith sub) true else t().hasSubSequence(sub)
      }*/

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, b)) {
    case (Cons(headA, tailA), Cons(headB, tailB)) => Some((f(headA(), headB()), (tailA(), tailB())))
    case _ => None
  }

  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, b)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
    case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  //Ex 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z))) { case (a, (x, stream)) =>
    val res = f(a, x)
    (res, cons(res, stream))
  }._2
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

  //Ex. 5.12
  def fibsUnfold: Stream[Int] = unfold((0, 1)) { case (n_2, n_1) => Some(n_1, (n_1, n_2 + n_1)) }

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def ones: Stream[Int] = constantUnfold(1)

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
    Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  )
}