package chapters.fifth

import chapters.fifth.Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t_) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil:List[A]
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h ,t) if(p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) if (p(h())) => t().forAll(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if(p(a)) cons(a, b) else empty)

  def headOption2: Option[A] = {
    foldRight(None:Option[A])((a,b) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((h,t) => cons(f(h), t))
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a,b) => cons(a, b))
  }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((a,b) => if(f(a)) cons(a, b) else b)

}
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends  Stream[A]



object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val s:Stream[A] = cons(a, s)
    s
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def fibs(): Stream[Int] = {
    def help(i: Int, d: Int): Stream[Int] = {
      cons(i, help(d, i + d))
    }
    help(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }
  }
}
