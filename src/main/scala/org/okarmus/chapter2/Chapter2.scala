package org.okarmus.chapter2

import scala.annotation.tailrec

object Chapter2 {

  //Ex. 2.1
  def fib(n: Int): Int = {
    require(n > 0)
    n match {
      case 1 => 0
      case 2 => 1
      case x => fib(x - 1) + fib(x - 2)
    }
  }

  //Ex. 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def inner(as: List[A]): Boolean = as match {
      case head :: next :: tail => if (ordered(head, next)) inner(next :: tail) else false
      case _ => true
    }

    inner(as.toList)
  }

  //Ex 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  //Ex 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  //Ex 2.5
  def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))

}


object Main extends App {

  import Chapter2._

  println(fib(1))
  println(fib(0))
  println(fib(2))
}