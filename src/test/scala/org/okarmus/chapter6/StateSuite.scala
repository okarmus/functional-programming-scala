package org.okarmus.chapter6

import org.scalatest.FunSuite

class StateSuite extends FunSuite{

  case class Incrementer(int: Int) {
    def nextInt: (Int, Incrementer) = {
      val x = int + 1
      (x, Incrementer(x))
    }
  }


  test("unit should return concrete value") {
    val state: State[Incrementer, Int] = State.unit(12)
    val incrementer = Incrementer(0)

    assert(state.run(incrementer) == (12, incrementer))

  }


  test("flat map should work properly") {
    val state: State[Incrementer, Int] = State(_.nextInt)

    val f: Int => State[Incrementer, String] = int => State(inc => ("value" + (int + 100).toString, inc))
    val x: State[Incrementer, String] = state.flatMap(f)

    println(x.run(Incrementer(0)))

  }



}
