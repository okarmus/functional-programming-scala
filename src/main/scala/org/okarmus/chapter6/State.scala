package org.okarmus.chapter6


//Ex. 6.10
case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A  => State[S, B]) = { s: S =>
    val (a, newS) = run(s)
    f(a).run(newS)
  }

}


object State {

  def unit[S, A](a: => A) : State[S, A] = State(s => (a, s))


}
