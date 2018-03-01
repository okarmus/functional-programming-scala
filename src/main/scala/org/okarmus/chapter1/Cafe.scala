package org.okarmus.chapter1

class CreditCard {
  def charge(price: Double): Unit = print("charged")
}

case class Charge(cc: CreditCard, price: Double) {

  def combine(other: Charge): Charge = {
    if (other.cc == cc) {
      Charge(cc, price + other.price)
    } else {
      throw new Exception("Can not combine charges to different cards.")
    }
  }
}

class Coffee {
  val price: Double = 12.0
}

class Cafe {

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee
    (cup, Charge(cc, cup.price))
  }

  def buyCoffee(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))

    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

}


object Main extends App {
  val cafe = new Cafe
  val creditCard = new CreditCard

  val result = cafe.buyCoffee(creditCard, 12)

  print(result)
}