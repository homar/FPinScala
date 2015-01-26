package chapters.fifth

object StreamRunner {

  def main(args: Array[String]) = {
    val s = Stream(1,2,3,4,5,6)
    println(s.toList)
    require(s.drop(5).toList == List(6))
    require(s.takeWhile(_ < 5).toList == List(1,2,3,4))

    def m(i: Int): Int = {
      println("map")
      i + 10
    }

  def f(i: Int): Boolean = {
    println("filter")
    i % 2 == 0
  }

    val t = s.map(m).filter(f)
    println("dupa1")
    println(t.toList);

    import Stream._
    println(constant(5).take(10).toList)
    println(from(1).take(10).toList)
    println(fibs().take(10).toList)
  }

}
