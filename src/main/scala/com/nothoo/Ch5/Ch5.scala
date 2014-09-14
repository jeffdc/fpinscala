package com.nothoo.Ch5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._
  // 5.1 -- will stack overflow with any decently sized stream
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  // 5.2
  def take(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h,t) if n == 1 => cons(h(), empty)
      case Cons(h,t) => cons(h(), t().take(n - 1))
    } else
      Stream()

  def drop(n: Int): Stream[A] = {
    @tailrec
    def loop(s: Stream[A], n: Int): Stream[A] =
      if (n <= 0) s
       else s match {
        case Cons(h, t) => loop(t(), n - 1)
        case _ => Stream()
      }
    loop(this, n)
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => Stream()
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}

object Ch5 extends App {
  // 5.1
  assert(List(1,2,3) == Stream(1,2,3).toList)

  // 5.2
  assert(List(1,2) == Stream(1,2,3).take(2).toList)
  assert(List(3) == Stream(1,2,3).drop(2).toList)

  // 5.3
  assert(List(1,2) == Stream(1,2,3,4).takeWhile(a => a <= 2).toList)
}
