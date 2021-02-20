package calculator

object main {


  def main(args: Array[String]) {
    println(args.mkString(" "))
    val calc = new CalcBase
    calc.run()
  }
}
