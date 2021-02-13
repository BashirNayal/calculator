package calculator

object main {
  def main(args: Array[String]) {
    println(args.mkString(" "))
    val calc = new CalcIntegral
    calc.run()
  }
}
//added a comment