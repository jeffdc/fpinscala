package com.nothoo.ch4

// hide stuff from standard library -- looks to be very hard to convince IDEA that we are nut using these types
import scala.{Either => _, Option => _, Some => _, None => _, Left => _, Right => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this map f getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

// make all of this stuff private so that the other exercises do not see them
object Option {
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa,bb)))

  // 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  // 4.5
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
  }
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}

sealed trait Either[+E,+A] {
  // 4.6
  def map[B](f: A => B): Either[E,B] = this match {
    case l@Left(_) => l
    case Right(x) => Right(f(x))
  }

  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case l@Left(_) => l
    case Right(x) => f(x)
  }

  def orElse[EE >: E, B >: A](b: Either[EE,B]): Either[EE,B] = this match {
    case Left(_) => b
    case r@Right(x) => r
  }

  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
}
case class Left[+E](value: E) extends Either[E,Nothing]
case class Right[+A](value: A) extends Either[Nothing,A]
object Either {
  // 4.7
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def traverse2[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E,A](as: List[Either[E,A]]): Either[E,List[A]] = traverse(as)(identity)


}


object Ch4 extends App {
  // 4.1
  assert(Some(2) == Some(1).map(_ + 1))
  assert(1 == Some(1).getOrElse(2))
  assert(2 == None.getOrElse(2))
  assert(Some(1) == Some(1).orElse(Some(2)))
  assert(Some(1) == None.orElse(Some(1)))
  assert(None == Some(1).filter({_ > 1}))

  // 4.2
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  // if m is the mean of xs, then the variance is the mean of math.pow(x-m,2) for each x in xs
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs map (x => math.pow(x - m,2))))
  }
  assert(variance(Seq(2,4)).map(_ == 1).getOrElse(false))

  // 4.4
  import Option._
  assert(Some(List(1,2,3)) == sequence(List(Some(1),Some(2),Some(3))))
  assert(None == sequence(List(Some(1),None,Some(3))))

  // 4.5
  val a = List(1,2,3)
  val f: (Int) => Option[Int] = { (x:Int) => if (x<2) Some(x) else None }
  assert(sequence(a map f) == traverse(a)(f))
  assert(sequence(List(Some(1),Some(2))) == sequence2(List(Some(1),Some(2))))

  // 4.6
  assert(Right(2) == Right(1).map(_ + 1))
  assert(Right(1) == Right(Right(1)).flatMap(identity))
  assert(Right(3) == Right(2).map2(Right(1))((a:Int,b:Int) => a+b))

  // 4.7
  assert(Right(List(1,2,3)) == Either.sequence(List(Right(1),Right(2),Right(3))))
}
