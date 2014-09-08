package com.nothoo

import scala.annotation.tailrec

object Ch2 extends App {

  // 2.1
  def fib(count: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
        if (n == 0)
          cur
        else
          loop(n - 1, cur, cur + prev)
    }
    loop(count - 2, 0, 1)
  }

  assert(fib(5) == 3)
  assert(fib(9) == 21)

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    as.deep == as.sortWith(ordered).deep
  }

  val sorter = (a: Int, b: Int) => a <= b
  assert(isSorted(Array(1,2,3), sorter))
  assert(!isSorted(Array(4,2,3), sorter))

  // 2.3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a: A, b: B) => f(a)(b)

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))


}