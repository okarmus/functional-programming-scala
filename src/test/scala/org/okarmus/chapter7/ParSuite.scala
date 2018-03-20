package org.okarmus.chapter7

import java.util.concurrent.Executors

import org.okarmus.chapter7.Par.Par
import org.scalatest.FunSuite

class ParSuite extends FunSuite {

  val es = Executors.newFixedThreadPool(10)

  test("map2 should work properly") {

    val parDouble: Par[Double] = Par.lazyUnit(2.0)
    val parInt: Par[Int] = Par.unit(1)

    val parString: Par[String] = Par.map2(parDouble, parInt)((d, i) => s"$d $i")

    assert(parString(es).get == "2.0 1")
  }

  test("should async evaluate function") {
    val f: Int => Boolean = x => x % 2 == 0

    val async: Int => Par[Boolean] = Par.asyncF(f)

    assert(async(12)(es).get)

    assert(!async(13)(es).get)
  }

  test("should sort list in parallel") {
    import Par._

    val list = List(9, 2, 1, 2, 5)
    val expected = List(1, 2, 2, 5, 9)


    assert(
      sortPar(unit(list))(es).get == expected
    )
  }

  test("par map should map list in parallel") {
    import Par._

    val list = List(1, 2, 3, 4)
    val expected = List("1", "2", "3", "4")

    assert(
      parMap(list)(_.toString)(es).get == expected
    )
  }

  test("par filter should work as expected") {
    import Par._

    val list = List(1, 2, 3, 4)
    val expected = List(2, 4)

    assert(
      parFilter(list)(_ % 2 == 0)(es).get == expected
    )
  }



}
