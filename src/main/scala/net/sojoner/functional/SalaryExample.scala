package net.sojoner.functional

object SalaryRules {

  def plusAllowence(x:Double) = x * 1.2
  def plusBonus(x:Double) = x * 1.1
  def plusTax(x:Double) = x * 0.7
  def plusSurcharge(x:Double) = x * 0.7

}

class SalaryCalculator (fs:List[Double => Double]=List()) {

  def wiff(f:Double => Double)={
    val newFunctions: List[(Double) => Double] = fs :+ f
    new SalaryCalculator(newFunctions)
  }

  def calculate(basic:Double)={
    fs
      .foldLeft(identity[Double] _)((f,g) => f.andThen(g))
      .apply(basic)
  }
}

object SalaryExample {

  implicit final class ExtendedBiFunction[T, U, R](val self: (T, U) ⇒ R) extends AnyVal {

    def compose1[V](before: V ⇒ T): (V, U) ⇒ R = (v, u) ⇒ self(before(v), u)
    def compose2[V](before: V ⇒ U): (T, V) ⇒ R = (t, v) ⇒ self(t, before(v))

  }

  // my bifunction in scala ;-(
  val converter = (_:Double)*(_:Double)

  def main(args: Array[String]): Unit = {

    val result: Double = converter.apply(1.609, 10)
    println("Apply result, " + result)

    val miles2KmConverter: (Double) => Double = converter.curried(1.609)
    val result2: Double = miles2KmConverter.apply(10)
    println("Apply2 curried result, " + result2)

    val convertedValues: List[Double] = List[Double](10, 20, 50).map(converter.curried(1.609))
    println("Converted values, "+ convertedValues)

    val convertedValuesC: List[Double] = List[Double](10, 20, 30).map(converter.curried(1.609).andThen(n => n+32))
    println("Converted values C°2F, "+ convertedValuesC)

    val f2cConverter: (Double) => Double = converter.compose2((n:Double)  => n - 32).curried(5.0/9)
    println("Converted F2C° with composition: "+f2cConverter(10))

    val calculator: SalaryCalculator = new SalaryCalculator()
    val res=calculator
      .wiff(SalaryRules.plusAllowence)
      .wiff(SalaryRules.plusBonus)
      .wiff(SalaryRules.plusTax)
      .wiff(n => n * 0.95)
      .calculate(1337.42)
    println("All there is:: "+res)
  }
}
