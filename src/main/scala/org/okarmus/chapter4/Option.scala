package org.okarmus.chapter4

import scala.annotation.tailrec

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }


  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]


object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case _: Exception => None
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(aVal), Some(bVal)) => Some(f(aVal, bVal))
  }

  //Ex. 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((maybeA, acc) => maybeA.flatMap(a => acc.map(a :: _))) //or use map2

  //Ex. 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((a, acc) => map2(f(a), acc)(_ :: _))

  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}

object Insurance {
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 1.0

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Option.Try(age.toInt)
    val optTickets = Option.Try(numberOfSpeedingTickets.toInt)

    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

}