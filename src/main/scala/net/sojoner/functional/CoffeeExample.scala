package net.sojoner.functional

object CoffeeExample {

  class CreditCard {

    def charge(price: Double): Unit = {
      println(s"SIDE EFFECT!!!!!! ${##} Booooo!!!")
    }
  }

  case class Coffee(name: String = "Venti Tall Latte with Grande Soy Frappucino Whip Cream Extra Beans") {

    def price: Double = 13.37
  }

  class BadBadCafe {

    def buyCoffee(cc: CreditCard): Coffee = {
      val coffee = Coffee()
      cc.charge(coffee.price)
      coffee
    }

    def buyCoffees(cc: CreditCard, n: Int): List[Coffee] = List.fill(n)(buyCoffee(cc))
  }

  case class Charge(cc: CreditCard, amount: Double) {

    def +(other: Charge) = {
      require(cc == other.cc, "Can't combine to different cards.")
      Charge(cc, amount + other.amount)
    }

    def charge(): Unit = cc.charge(amount)
  }

  class GoodCafe {

    def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
      val coffee = Coffee()
      (coffee, Charge(cc, coffee.price))
    }

    def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
      val (coffees, charges) = List.fill(n)(buyCoffee(cc)).unzip
      (coffees, charges.foldLeft(Charge(cc, 0))(_ + _))
    }
  }

  def main(args: Array[String]): Unit = {
    val bad = new BadBadCafe
    val bad_coffee = bad.buyCoffee(new CreditCard)
    val bad_coffees = bad.buyCoffees(new CreditCard, 5)
    println(s"coffees = ${bad_coffee :: bad_coffees}")
    println(s"=====================================")
    val good = new GoodCafe
    val (good_coffee, good_charge1) = good.buyCoffee(new CreditCard)
    val (good_coffees, good_charge2) = good.buyCoffees(new CreditCard, 5)
    println(s"coffees = ${good_coffee :: good_coffees}")
    try {
      val finalCharge = good_charge1 + good_charge2
      finalCharge.charge()
    } catch {
      case ex: Throwable ⇒ println(s"ohoh: ${ex.getMessage}")
        good_charge1.charge()
        good_charge2.charge()
    }
  }
}
