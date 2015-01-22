package chapters.fourth

object OtherFunctions {
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None}
  }

  def mean(s: Seq[Double]) = if (s.isEmpty) None else Some(s.sum/s.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f:(A,B) => C): Option[C] = {
    a flatMap(aa => (b map (f(aa, _))))
  }

  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    l match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(hh => sequence(t) map (hh :: _))
    }
  }

  def sequenceWithFold[A](l: List[Option[A]]): Option[List[A]] = {
     l.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a,b)(_ :: _))
  }

  def traverse[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] = {
    l.foldLeft[Option[List[B]]](Some(Nil))((b,a) => b flatMap (bb => f(a) map (bb :+ _)))
  }

  def traverse2[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] = {
    l match {
      case Nil => Some(Nil)
      case h :: t => f(h) flatMap(b => traverse2(t)(f) map (b :: _))
    }
  }
}
