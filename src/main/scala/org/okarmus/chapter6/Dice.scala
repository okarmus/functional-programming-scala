package org.okarmus.chapter6

import scala.util.Random

object Dice {

  def rollDice: Int = {
     val rng = new Random
    rng.nextInt(6)
  }
}


object Main extends App {
  1 to 6 map(_ => Dice.rollDice) foreach println
}
