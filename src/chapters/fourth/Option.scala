package chapters.fourth

import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map (f) getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(x => if(f(x)) Some(x) else None)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
