package chapters.fourth

import scala.{Either => _, Option => _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this map(f) getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(v) => v
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map(x => Some(x)) getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(x => if(f(x)) Some(x) else None)
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
