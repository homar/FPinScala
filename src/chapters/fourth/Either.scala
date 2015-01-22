package chapters.fourth

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(v) => Left(v)
      case Right(v) => Right(f(v))
    }
  }

  def orElse[EE >: E, B >: A](z: => Either[EE, B]): Either[EE,B] = {
    this match {
      case Left(v) => z
      case Right(v) => Right(v)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
   this match {
      case Left(v) => Left(v)
      case Right(v) => f(v)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE,C] = {
    this flatMap(a => b map (bb => f(a, bb)))
  }

  def map2For[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE,C] = {
    for{aa <- this; bb <- b} yield (f(aa,bb))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
