package chapters.fourth

object Runner {

  def main(args: Array[String]) = {
    def res(t: Int): Option[String] = if(t == 1) Some("1") else None
    def sres(t: String): Option[Int] = if(t == "1") Some(5) else None
    def sres2(t: String): Option[Int] = if(t == "3") Some(5) else None

    val optSome = res(1)
    val optNone = res(0)
    val optSome2 = sres2("3")

    def extract[A](t: Option[A]): String  = {
      t match {
        case Some(c) => c.toString
        case _ => ":("
      }
    }

    println(extract(optSome))
    println(extract(optNone))

    println(optSome  getOrElse ":)")
    println(optNone getOrElse ":(")

    println(extract(optSome.flatMap(sres)))
    println(extract(optSome.flatMap(sres2)))
    println(extract(optNone.flatMap(sres)))

    println(extract(optSome orElse optNone))
    println(extract(optNone orElse optSome))

    println(extract(optSome2 filter (x => x == 5)))
    println(extract(optSome2 filter (x => x == 4)))
    println(extract(optNone filter(_ => true)))
  }

}
