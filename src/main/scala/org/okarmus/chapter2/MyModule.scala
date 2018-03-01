package org.okarmus.chapter2

import scala.annotation.tailrec

object MyModule {

  def factorial(n: Int): Int = {
    @tailrec
    def inner(n: Int, acc: Int): Int = n match {
      case 0 => acc
      case _ => inner(n - 1, n * acc)
    }

    inner(n, 1)
  }

  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    formatResult("absolute value", x, abs)
  }

  private def formatFactorial(n: Int) = {
    formatResult("factorial", n, factorial)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }
}
