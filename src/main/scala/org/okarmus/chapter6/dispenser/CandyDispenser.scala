package org.okarmus.chapter6.dispenser


//Ex. 6.11
case class Machine(locked: Boolean, candies: Int, coins: Int)

sealed trait Input

case object Coin extends Input

case object Turn extends Input


object CandyDispenser {

  def update(machine: Machine, input: Input): Machine = (machine, input) match {
    case (Machine(_, 0, _), _) => machine
    case (Machine(true, _, coins), Coin) => machine.copy(locked = false, coins = coins + 1)
    case (Machine(false, candies, _), Turn) => machine.copy(locked = true, candies = candies - 1)
    case (Machine(true, _, _), Turn) => machine
    case (Machine(false, _, _), Coin) => machine
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine => {
    val newMachine: Machine = inputs.foldLeft(machine)((machine, input) => update(machine, input))    //TODO maybe for comprehension could be used
    ((newMachine.candies, newMachine.coins), newMachine)
  })

}

object Simulation extends App {
  val input = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
  val initialMachine = Machine(locked = true, candies = 5, coins = 10)

  val (result, machine) = CandyDispenser.simulateMachine(input).run(initialMachine)

  println(s"candies: ${result._1} coins: ${result._2}")
  println(s"machine state $machine")

}