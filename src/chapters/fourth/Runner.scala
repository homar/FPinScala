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

    require(extract(optSome)=="1")
    require(extract(optNone)==":(")

    require((optSome  getOrElse ":)") == "1")
    require((optNone.getOrElse(":(")) == ":(")

    require(extract(optSome.flatMap(sres)) == "5")
    require(extract(optSome.flatMap(sres2)) == ":(")
    require(extract(optNone.flatMap(sres)) == ":(")

    require(extract(optSome orElse optNone) == "1")
    require(extract(optNone orElse optSome) == "1")

    require(extract(optSome2 filter (x => x == 5)) == "5")
    require(extract(optSome2 filter (x => x == 4)) == ":(")
    require(extract(optNone filter(_ => true)) == ":(")
  }

}
