package org.okarmus.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  case class SimpleRNG(seed: Long) extends RNG {

    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  //Ex. 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, newRng) = rng.nextInt
    if (int < 0) (-(int + 1), newRng) else (int, newRng)
  }

  //Ex. 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (int, newRng) = rng.nextInt
    (int / (Integer.MAX_VALUE.toDouble + 1), rng)
  }

  //Ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)

    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (1 to count).foldLeft((Nil: List[Int], rng)) { case ((list, r), _) =>
      val (i, nextRng) = r.nextInt
      (i :: list, nextRng)
    }

  def unit[A](a: A): Rand[A] = rnd => (a, rnd)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven(rng: RNG): Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  //Ex. 6.5
  def doubleMap(rng: RNG): Rand[Double] = {
    map(_.nextInt)(_ / (Integer.MAX_VALUE.toDouble + 1))
  }

  //Ex 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))


  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  //Ex. 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((acc, f) => map2(f, acc)((x, y) => y :: x))
  }

  def intsSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2) else nonNegativeLessThan(n)(rng)
  }

  //Ex. 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  //Ex. 6.9
  def mapUsingFlat[A, B](f: Rand[A])(g: A => B): Rand[B] = flatMap(f)(a => unit(g(a)))

  def map2UsingFlat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))
}

object Main2 extends App {

  import RNG._

  val initial = SimpleRNG(42)

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  /*
    println(initial.nextInt._1)
    println(initial.nextInt._1)

    println(initial.nextInt._2.nextInt._1)


    println(nonNegativeInt(initial.nextInt._2)._1)
    println(double(initial)._1)*/


  println(ints(10)(initial)._1)

  println(intsSequence(10)(initial)._1)

}