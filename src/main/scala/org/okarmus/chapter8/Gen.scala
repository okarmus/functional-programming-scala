package org.okarmus.chapter8

//TODO IMPLEMENT IT ONE DAY

import java.util.concurrent.{ExecutorService, Executors}

import org.okarmus.chapter5.Stream
import org.okarmus.chapter6.RNG.SimpleRNG
import org.okarmus.chapter6.{RNG, State}
import org.okarmus.chapter7.Par
import org.okarmus.chapter7.Par.Par
import org.okarmus.chapter8.Gen.{buildMsg, randomStream}
import org.okarmus.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(b.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] = map2(g)((_,_))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  //Ex. 8.10
  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
    State(RNG.nonNegativeLessThan(stopExclusive - start))
      .map(_ + start)
  )

  def int: Gen[Int] = Gen(State(_.nextInt))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def pair[A](g: Gen[A]): Gen[(A, A)] = listOfN(2, g).map(l => (l.head, l.last))

  def stringOfN(n: Int): Gen[String] = listOfN(n, choose(97, 123)).map(ints => ints.map(_.toChar).mkString)

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(size => listOfN(size, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(size => listOfN(size max 1, g))

  def randomStream[A](value: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(value.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception) = {
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample))
  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(other: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => other.run(max, n, rng)
      case failure => failure
    }
  }

  def ||(other: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(msg, _) => other.tag(msg).run(max, n, rng)
      case x => x
    }
  }

  def tag(msg: String): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

      val prop: Prop = props.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    randomStream(as)(rng).zipWith(Stream.from(0).take(n))((a, i) => (a, i)).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)
         ): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests: \n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println("+ OK, proved property")
    }
  }

  def checkPar(p: => Par[Boolean]): Prop = {
    forAllPar(Gen.unit(()))(_ => p)
  }

  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p1, p2)(_ == _)

  val S = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
    forAll(S ** g) { case (s, a) => f(a)(s).get }
  }

}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
}


object Example1 extends App {
  val rng = SimpleRNG(40)

  val int = Gen.unit(12)

  println(Gen.pair(int).sample.run(rng)._1 == (12, 12))

  println(Gen.stringOfN(12).sample.run(rng)._1)


  val intList = Gen.listOfN(10, Gen.choose(0, 100)) //TODO change it to listOf
  val prop = Prop.forAll(intList)(ns => ns.reverse.reverse == ns) && Prop.forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

  val failingProp = Prop.forAll(intList)(ns => ns.reverse == ns)

  println(prop.run(10, 12, rng))
  println(failingProp.run(5, 10, rng))
}


object Example2 extends App {
  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  Prop.run(maxProp)
}

object ListSorted extends App {
  val smallInt = Gen.choose(-10, 10)
  val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
    val sorted = ns.sorted
    sorted.isEmpty || sorted.tail.isEmpty || !sorted.zip(sorted.tail).exists { case (a, b) => a > b }
  }

  Prop.run(sortedProp)
}

object ParallelLaws extends App {
  val es: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i => Par.map(i)(_ + 1)(es).get == Par.unit(2)(es).get)

  Prop.run(p1)
}

object ParallelLawsProperty extends App {
  val es: ExecutorService = Executors.newCachedThreadPool

  val p2 = Prop.checkPar {
    Prop.equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }


  Prop.run(p2)

}