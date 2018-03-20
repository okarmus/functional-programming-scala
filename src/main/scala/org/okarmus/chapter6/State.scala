package org.okarmus.chapter6

import org.okarmus.chapter6.RNG.SimpleRNG


//Ex. 6.10
case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, newS) = run (s)
    f(a).run(newS)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))


  def modify(f: S => S): State[S, A] = State(s => {
    val (a, newS) = run(s)
    (a, f(newS))
  })


}

object State {

  def unit[S, A](a: => A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}


object Run extends App {

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def ints(length: Int): List[Int] = List.fill(length)(12)

  println(int.run(SimpleRNG(24))._1)

}