package chapters.fourth

object OtherFunctionsEither {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil => Right(Nil:List[A])
      case h :: t => h flatMap (a => sequence(t) map(tt => a::tt))
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil:List[B])
      case h :: t => f(h) flatMap(bb => traverse(t)(f) map(lb => bb :: lb))
    }
  }

  def seqAsTrav[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }

}
