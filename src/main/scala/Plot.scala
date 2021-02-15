package calculator
import processing.core.PApplet
object Plot {
  def plot(f : Fun): Unit = {
    val processingArgs = Array("Plot")
    val mySketch = new Plot(f)
    PApplet.runSketch(processingArgs, mySketch)
  }
}
class Plot(f : Fun) extends PApplet {

  override def setup(): Unit = {
//    println(args.mkString(" "))
  }

  override def mousePressed() : Unit = {
//    exit()
  }

  override def settings(): Unit = {
    size(800, 800)
  }

  override def draw(): Unit = {
    background(190)
    fill(128)
    var origin : Point = Point(width/2,height/2)
    draw_axis(origin.x , origin.y)

    draw_graph(f)
  }
  def draw_axis(x : Float , y : Float) {
    val center : Point = Point(x, y)
//    line(width/2 + x , 0 , width/2 + x, height)
    line(x , 0 ,  x, height)

//    line(0 , height/2 - y , width , height / 2 - y) // minus because draw() has it's axis center at the top left
      line(0 , y , width ,  y) // minus because draw() has it's axis center at the top left

    for(i <- 0 to 20) {
      circle(Point(center.x , 0 + i * height * 0.05f) , 4)
      circle(Point(0 + i * width * 0.05f , center.y) , 4)

    }
  }
  def circle(p : Point , r : Float) = ellipse(p.x , p.y , r , r)
  def draw_graph(f : Fun): Unit = {
    var i = 0
    val max = 1000
    var x = -500
    var arr = new Array[Point](max)
    while(i < max) {
      arr(i) = Point(x.toFloat , f.solve_for(Constant(x)).value.toFloat)
      i += 1
      x += 1
    }
    arr = arr.map(p => Point((p.x*10 + width/2), ( height / 2 - p.y*20) ))
//    for(i <- 0 until arr.length - 1) println(arr(i))
    for(i <- 0 until arr.length - 1) line(arr(i).x , arr(i).y , arr(i + 1).x , arr(i + 1).y)
//    for(i <- 0 until arr.length - 1) rect(arr(i).x , arr(i).y , 2 , 2)
  }

  def translate_coords(x : Double , y : Double) : Array[Double] = {
    var arr = new Array[Double](2)
    arr(0) =  x + width/2
    arr(1) = y - height/2
    arr
    Array(x , y)
  }

  def line(p1 : Point , p2 : Point) : Unit = line(p1.x , p1.y , p2.x , p2.y)
}
