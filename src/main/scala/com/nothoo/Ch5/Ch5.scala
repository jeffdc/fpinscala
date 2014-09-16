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
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Stream()
  }

  def foldRight[B](x: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(x)(f))
    case _ => x
  }

  // as the text points out, this is not stack safe since foldRight is not tail-recursive
  def exists(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)

  // 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  // 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else empty)

  // 5.6
  def headOptionOriginal: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }
  def headOption: Option[A] = foldRight(None: Option[A])((a,_) => Some(a))

  // 5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(f(a),b))
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else empty)
  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a,b) => cons(a,b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b) => f(a) append b)

  // 5.13
  def map13[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h,t) => Some(f(h()), t())
    case _ => None
  }
  def take13(n: Int): Stream[A] = unfold(this, n) {
    case (Cons(h,t), n) if n == 1 => Some(h(), (empty,n-1))
    case (Cons(h,t), n) if n > 0  => Some(h(), (t(),n-1))
    case _ => None
  }
  def takeWhile13(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) if p(h()) => Some(h(),t())
    case _ => None
  }
  def zipWith[B,C](o: Stream[B])(f: (A,B) => C): Stream[C] = unfold(this,o) {
    case (Cons(lh,lt), Cons(rh,rt)) => Some((f(lh(), rh()), (lt(), rt())))
    case _ => None
  }
  // this one is a toughy, need to think about it more
//  def zipAll[B](o: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this,o) {
//    case (Empty, Empty) => None
//    case (Cons(h,t), Empty) => Some((h(),t()), )
//  }

  // 5.14 -- this is ugly, probably need zipAll
  def startsWith[A](o: Stream[A]): Boolean = zipWith(o)(_ == _).takeWhile(b => b).toList.size > 0

  // 5.15
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some(s, s drop 1)
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists(_ startsWith s)

  // 5.16 - not sure, does not seem that unfold is correct
  def scanRight[B](x: => B)(f: (A, => B) => B): Stream[B] = ???
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

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  // 5.8
  def constant[A](a: A): Stream[A] = cons(a,constant(a))

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // 5.11
  def from2(n: Int): Stream[Int] = unfold(n){ x => Some(x, x+1) }

  // 5.12
  def constant2[A](a: A): Stream[A] = unfold(a){ x => Some(x,x) }
}

object Ch5 extends App {
  val TEST_STREAM: Stream[Int] = Stream(1, 2, 3)
  // 5.1
  assert(List(1,2,3) == TEST_STREAM.toList)

  // 5.2
  assert(List(1,2) == TEST_STREAM.take(2).toList)
  assert(List(3) == TEST_STREAM.drop(2).toList)

  // 5.3
  assert(List(1,2) == Stream(1,2,3,4).takeWhile(a => a <= 2).toList)

  // 5.4
  assert(TEST_STREAM.forAll(_ < 10))
  assert(!TEST_STREAM.forAll(_ < 2))

  // 5.5
  assert(TEST_STREAM.takeWhile(_ < 3).toList == TEST_STREAM.takeWhile2(_ < 3).toList)

  // 5.6
  assert(Stream(1).headOptionOriginal == Stream(1).headOption)
  assert(Empty.headOptionOriginal == Empty.headOption)

  // 5.7
  assert(List(2,4,6) == TEST_STREAM.map(_ * 2).toList)
  assert(List(1,2) == TEST_STREAM.filter(_ < 3).toList)
  assert(List(1,2,3,1,2,3) == TEST_STREAM.append(TEST_STREAM).toList)
  assert(List(1,2,3) == Stream(TEST_STREAM).flatMap(identity).toList)

  import Stream._
  // 5.8
  assert(List(1,1,1) == constant(1).take(3).toList)

  // 5.9
  assert(List(1,2,3) == from(1).take(3).toList)

  // 5.10
  val fibs: Stream[Int] = {
    def loop(n: Int, n1: Int): Stream[Int] = cons(n, loop(n1, n+n1))
    loop(0,1)
  }
  assert(List(0,1,1,2,3,5) == fibs.take(6).toList)

  // 5.11
  assert(fibs.take(6).toList == unfold((0,1)){ case (n,n1) => Some((n,(n1,n+n1))) }.take(6).toList)

  // 5.12
  val fibs2: Stream[Int] = unfold((0,1)){ case (n,n1) => Some((n,(n1,n+n1))) }
  assert(fibs.take(6).toList == fibs2.take(6).toList)
  assert(from(1).take(3).toList == from2(1).take(3).toList)
  assert(constant(1).take(4).toList == constant2(1).take(4).toList)

  val ones: Stream[Int] = constant2(1)
  assert(List(1,1,1) == ones.take(3).toList)

  // 5.13
  assert(TEST_STREAM.map13(identity).toList == TEST_STREAM.map(identity).toList)
  assert(TEST_STREAM.take13(2).toList == TEST_STREAM.take(2).toList)
  assert(TEST_STREAM.takeWhile13(_>2).toList ==TEST_STREAM.takeWhile(_>2).toList)
  assert(List("ab", "cd") == Stream("a", "c").zipWith(Stream("b", "d"))(_ + _).toList)

  // 5.14
  assert(TEST_STREAM startsWith Stream(1,2))
  assert(!TEST_STREAM.startsWith(Stream(2,3)))
  assert(TEST_STREAM startsWith Stream(1))
  assert(!TEST_STREAM.startsWith(empty))

  // 5.15
  assert(List(List(1,2,3),List(2,3),List(3), List()) == Stream(1,2,3).tails.map(_.toList).toList)

  // 5.16
  //TODO
//  assert(List(6,5,3,0) == Stream(1,2,3).scanRight(0)(_+_).toList)

}
