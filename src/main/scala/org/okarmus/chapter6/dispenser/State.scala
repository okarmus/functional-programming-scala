package org.okarmus.chapter6.dispenser

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, newS) = run(s)
    f(a).run(newS)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](stateB: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => stateB.map(b => f(a, b)))

}


object State {

  def unit[S, A](a: => A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((state, acc) => state.map2(acc)(_ :: _))
}
