package org.okarmus.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import org.okarmus.chapter7.Par.Par


object Par {

  type Par[A] = ExecutorService => Future[A]

  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] = es => {
    val a = parA(es)
    val b = parB(es)
    UnitFuture(f(a.get, b.get))
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork({
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  })

  //Ex. 7.5
  def sequence[A](pars: List[Par[A]]): Par[List[A]] =
    pars.foldRight(unit(Nil): Par[List[A]])((a, b) => map2(a, b)(_ :: _))


  //Ex 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val mapped: List[Par[List[A]]] = as.map(asyncF(a => if(f(a)) List(a) else Nil))
    map(sequence(mapped))(x => x.flatten)
  }
}

class Main extends App {

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 0) {
      Par.unit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

}