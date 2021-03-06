package chapters.third

object Tree {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
      t match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
      }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f:A => B)(g:(B,B) => B):B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1+_+_)
  }

  def maximumFold(t: Tree[Int]): Int = {
    fold(t)(x => x)(_ max _)
  }

  def depthFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((d1,d2) => 1 + (d1 max d2))
  }

  def mapFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
  }

  def main(args: Array[String]) = {
    val st = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    val it = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))

    println(size(st))
    println(maximum(it))
    println(depth(st))
    println(depth(it))
    println(map(it)(_+1))

    println(sizeFold(st))
    println(maximumFold(it))
    println(depthFold(st))
    println(depthFold(it))
    println(mapFold(it)(_+1))
  }

}
