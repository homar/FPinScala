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

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t), n) if n == 1 => Some((h(), (empty, n-1)))
      case (Cons(h,t), n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this){
      case Cons(h, t) if(p(h())) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B,C](l: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, l)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None:Option[B]), (t1(), Empty)))
      case (Empty, Cons(h2, t2)) => Some(((None:Option[A], Some(h2())), (Empty, t2())))
      case _ => None
    }
  }

  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile(!_._2.isEmpty).forAll{
      case (Some(x), Some(y)) => x == y
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this){
      case Cons(h, t) => Some((Cons(h, t), t()))
      case _ => None
    } append( Stream(empty))
  }
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

  def fibsViaUnfold(): Stream[Int] = {
    unfold((0,1)){case (t0, t1) => Some(t0, (t0, t0+t1))}
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(a => Some(a, a+1))
  }

  def constanViaUnfold[A](a: A): Stream[A] = {
    unfold(a:A)(x => Some(x,x))
  }

  def onesViaUnfold(): Stream[Int] = {
    unfold(1)(_ => Some(1, 1))
  }

}
