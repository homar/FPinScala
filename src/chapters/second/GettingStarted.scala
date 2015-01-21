package chapters.second



object GettingStarted {

  def isSorted[A](a: Array[A], f: (A,A) => Boolean) = {

    @annotation.tailrec
    def loop(index: Int): Boolean  = {
      if(index >= a.length) true
      else if(!f(a(index-1), a(index))) false
      else loop(index+1)
    }

    loop(1)
  }

  def partial1[A,B,C](a:A, f: (A,B) => C): B => C = {
    b => f(a,b)
  }

  def curry[A,B,C](f:(A,B) => C): A => (B => C) = {
    a => (b => f(a,b))
  }

  def uncurry[A,B,C](f: A => (B => C)): (A,B) => C = {
    (a,b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }


  def main(args: Array[String]) = {

    val testArray = Array(1,2,3,2,5)
    val sortedArray = Array(1,2,3,4,4,5)

    println(isSorted(testArray, (x: Int,y: Int) => y >= x))
    println(isSorted(sortedArray, (x: Int,y: Int) => y >= x))

  }

}
