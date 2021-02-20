package calculator
import processing.core.PApplet
import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

object Window {
  def main(args: Array[String] ) {
    draw(Plot.deserialize())
  }
  def draw(f : Fun): Unit = {
    val processingArgs = Array("Window")
    val mySketch = new Window(f)
    PApplet.runSketch(processingArgs, mySketch)
  }
}
class Window(f : Fun) extends PApplet {



  var origin : Point = Point(width/2,height/2)
  override def settings(): Unit = {
    size(800, 800)
  }
  val x_scale : Float = 1
  val y_scale : Float = 1
  var min_x : Double = 0
  var min_y : Double = 0
  val axis_drawn : Boolean = false
  override def draw(): Unit = {
    background(190)
    fill(128)
    val origin : Point = Point(width/2,height/2)
    draw_axis(origin.x , origin.y)

    draw_graph(f)
  }
  def draw_axis(x : Float , y : Float) {
    val center : Point = Point(x, y)
    line(x , 0 ,  x, height)

      line(0 , y , width ,  y) // minus because draw() has it's axis center at the top left

    for(i <- 0 to 20) {
      circle(Point(center.x , 0 + i * height * 0.05f) , 4)
      circle(Point(0 + i * width * 0.05f , center.y) , 4)

    }
  }
  def circle(p : Point , r : Float): Unit = ellipse(p.x , p.y , r , r)
  def draw_graph(f : Fun): Unit = {

    fill(0)
    var i = 0
    val max = 1000
    var x = -500
    var arr = new Array[Point](max)
    while(i < max) {
      arr(i) = Point(x.toFloat , f.solve_for_x(Constant(x)).value.toFloat)
      i += 1
      x += 1
    }
    arr = arr.map(p => Point(p.x*10 + width/2,  height / 2 - p.y*10))
    for(i <- 0 until arr.length - 1) line(arr(i).x , arr(i).y , arr(i + 1).x , arr(i + 1).y)
  }



  def line(p1 : Point , p2 : Point) : Unit = line(p1.x , p1.y , p2.x , p2.y)
}



object Plot {

  @throws[Exception]
  def plot(f : Fun , blocking : Boolean = true): Unit = {//This creates a new jvm
    serialize(f)

    val separator = System.getProperty("file.separator")
    val classpath = System.getProperty("java.class.path")
    val path = System.getProperty("java.home") + separator + "bin" + separator + "java"
    val processBuilder = new ProcessBuilder(path, "-cp", classpath, classOf[Window].getName)
    val process = processBuilder.start

    if(blocking)process.waitFor
  }
  def serialize(f : Fun): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream("function"))
    oos.writeObject(f)
    oos.close()
  }
  def deserialize() : Fun = {
    val ois = new ObjectInputStream(new FileInputStream("function"))
    val f = ois.readObject.asInstanceOf[Fun]
    ois.close()
    f
  }

}
