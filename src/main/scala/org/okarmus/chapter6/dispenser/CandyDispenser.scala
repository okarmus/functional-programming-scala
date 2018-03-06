package org.okarmus.chapter6

//Ex. 6.11

case class Machine(locked: Boolean, candies: Int, coins: Int)

sealed trait Input

case object Coin extends Input

case object Turn extends Input


object Candy {


  private def update(state: State[Machine, (Int, Int)], input: Input): State[Machine, (Int, Int)] = (state, input) match {
    case (State(Machine(_, 0, _)), _) => state                                        //No candies we are ignoring an input
    case (State(Machine(true, _, coins)), Coin) =>  state.modify(_.copy(locked = false, coins = coins + 1))  //Inserting a coin into locked machine
    case (State(Machine(false,candies, coins)), Turn) => 
  }


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs.foldLeft(State.unit[Machine, (Int, Int)]((0, 0)))((x, y) => update(x, y))
}


object Simulation extends App {
  val input = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
  val initialMachine = Machine(locked = true, candies = 5, coins = 10)

  val finalState = Candy.simulateMachine(input).run(initialMachine)

  println("Machine state is: " + finalState._2)

}