package org.okarmus.chapter2

object Partial {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

}
