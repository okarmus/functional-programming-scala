package org.okarmus.chapter4

sealed trait Either[+E, +A]{
  //Ex. 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(x) => Left(x)
    case Right(x) => Right(f(x))
  }

  def flatMap[B, EE >: E](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(x) => Left(x)
    case Right(x) => f(x)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(x) => Right(x)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Left(x), _) => Left(x)
    case (_, Left(y)) => Left(y)
    case (Right(x), Right(y)) => Right(f(x, y))
  }

  def map2ForComprehension[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a1 <- this
      b1 <- b
    } yield f(a1, b1)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


object Either {

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {case e: Exception => Left(e)}
  }

  //Ex. 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[E, A](as: List[Either[E,A]]): Either[E, List[A]] = traverse(as)(x => x)

}