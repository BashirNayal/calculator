package calculator

trait CALCULATOR {

  def readEval(command : String) : String

  def run() : Unit = {
    while(true) {
      print(s"Calculator>> ")
      val s = scala.io.StdIn.readLine().trim

      if(s == "exit") return
      println(readEval(s))
    }
  }
}
