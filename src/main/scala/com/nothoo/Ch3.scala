package com.nothoo

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => t
  }

  // 3.3
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Cons(a, Nil)
    case Cons(h,t) => Cons(a,t)
  }

  // 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l) , n-1)
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => if (!f(h)) l else dropWhile(t, f)
  }

  // 3.6 -  what a kludgey mess
  def init[A](l: List[A]): List[A] = {
    def loop(l: List[A], m: List[A]): List[A] = {
      l match {
        case Nil => m
        case Cons(h, Nil) => m
        case Cons(h,t) => loop(t, Cons(h, m))
      }
    }
    reverse(loop(l, Nil))
  }

  // helper for 3.6
  def reverse[A](l: List[A]): List[A] = {
    def loop(l: List[A], m: List[A]): List[A] = l match {
      case Nil => m
      case Cons(h,t) => loop(t, Cons(h, m))
    }
    loop(l, Nil)
  }
}

object Ch3 extends App {
  import List._

  // 3.2
  assert(List(1,2,3) == tail(List(0,1,2,3)))
  assert(Nil == tail(Nil))
  assert(Nil == tail(List(1)))

  // 3.3
  assert(List(1,2,3) == setHead(List(2,2,3), 1))
  assert(List(1) == setHead(Nil, 1))

  // 3.4
  assert(List(3) == drop(List(1,2,3), 2))

  // 3.5
  assert(List(4,5) == dropWhile(List(1,2,3,4,5), (a:Int) => a < 4))

  // 3.6
  assert(List(1,2,3) == init(List(1,2,3,4)))
}
