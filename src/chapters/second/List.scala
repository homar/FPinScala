package chapters.second

import scala.annotation.tailrec

object List {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  def sum(xs: List[Int]): Int = {
    xs match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def apply[A] (as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: List[A]): List[A] = {
    xs match {
      case Nil => Nil
      case Cons(_, xt) => xt
    }
  }

  def setHead[A](a: A, xs: List[A]): List[A] = {
    xs match {
      case Nil => Cons(a, Nil)
      case Cons(x, t) => Cons(a, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def loop(i: Int, xs: List[A]): List[A] = {
      if( i <= 0) xs
      else loop(i - 1, tail(xs))
    }
    loop(n, l)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, t) if(f(x)) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, t) => Cons(x, init(t))
    }
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
    l match {
      case Nil => z
      case Cons(x, t) => f(x, foldRight(t, z)(f))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, b) => b + 1)
  }

  @tailrec
  def foldLeft[A,B](as: List[A], z:B)(f: (B,A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, t) => foldLeft(t, f(z, x))(f)
    }
  }

  def sumFoldLeft(l: List[Int]) = {
    foldLeft(l, 0)((x,y) => x + y)
  }

  def prodFoldLeft(l: List[Int]) = {
    foldLeft(l, 1)(_*_)
  }

  def lengthFoldLeft[A](l: List[A]): Int = {
    foldLeft(l, 0)((x,_) => x + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((b,a) => Cons(a, b))
  }

  def foldRight2[A,B](l: List[A], z: B)(f:(A,B)=>B): B = {
    foldLeft(reverse(l),z)((a,b) => f(b,a))
  }

  def foldLeft2[A,B](l: List[A], z:B)(f:(B,A) => B): B = {
    foldRight(reverse(l), z)((b,a) => f(a,b))
  }

  def append[A](l: List[A], z: List[A]): List[A] = {
    foldRight(l, z)((x, y) => Cons(x, y))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def trans_1(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((a,b)=> Cons(a+1, b))
  }

  def transformToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))
  }

  def map[A,B](as: List[A])(f: A=>B): List[B] = {
    foldRight(as, Nil: List[B])((a,b) => Cons(f(a),Nil))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, Nil:List[B])((b,a) => append(b, f(a)))
  }

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if(f(a)) Cons(a, Nil) else Nil)
  }

  def zip(l: List[Int], k: List[Int]): List[Int] = {
    l match {
      case Nil => k match {
        case Nil => Nil
        case x => x
      }
      case Cons(h, t) => k match {
        case Nil => Cons(h, t)
        case Cons(j, z) => Cons(h + j, zip(t, z))
      }
    }
  }

  def zipWith[A,B,C](l: List[A], k: List[B])(f:(A,B) => C): List[C] = {
    (l, k) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
    }
  }

  def subsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def doesMatch (a: List[A], b: List[A]): Boolean = {
      (a,b) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(h1,t1), Cons(h2,t2)) => h1==h2 && doesMatch(t1,t2)
      }
    }

    (sup, sub) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1,t1), sub) => if (doesMatch(Cons(h1, t1), sub)) true else subsequence(t1, sub)
    }

  }

  def main(args: Array[String]) = {

    val l = List(1,2,3,4,5)
    val l2 = List(6,7,8,9,10)
    val doubleList = List(1.0, 2.0, 3.0, 4.0, 5.0)
    val mergedList = Cons(l, Cons(l2, Nil))
    val x = l match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    println(x)

    println(tail(l))

    println(setHead(2, l))

    println(drop(l, 2))

    println(dropWhile[Int](l, x => x < 3))

    println(init(l))

    println(foldRight(l, Nil:List[Int])(Cons(_,_)))

    println(length(l))

    println(reverse(l))

    println(append(l, Cons(6, Nil)))

    println(concat(mergedList))

    println(trans_1(l))

    println(transformToString(doubleList))

    map(doubleList)(println(_))

    println(filter(l)(_ % 2 == 0))

    println(flatMap(l)(i => List(i ,i)))

    println(filterFlatMap(l)(_ % 2 == 0))

    println(zip(l, l2))

    println(zipWith(l, l2)((a,b) => a+b))

    println(subsequence(l, List(1,2)) && subsequence(l, List(2,3)) && subsequence(l, List(4)))

    println(subsequence(l, List(5,6)))

  }
}

