package com.nothoo.ch3

import scala.annotation.tailrec

// make all of this stuff private so that the other exercises do not see them
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

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // 3.10
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  // 3.11
  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product(l: List[Int]): Int = foldLeft(l, 1)( _ * _)
  def len[A](l: List[A]): Int = foldLeft(l, 0)((acc,_) => acc + 1)

  // 3.12
  def reverseF[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A]) { (acc,x) => Cons(x,acc) }

  // 3.13
  def foldLeft2[A,B](as: List[A], z: B)(f: (B,A) => B): B = foldRight(as, (b:B) => b)((x,acc) => b => acc(f(b,x)))(z)
  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B = foldLeft(as, (b:B) => b)((acc,x) => b => acc(f(x,b)))(z)

  // 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight2(a1, a2)(Cons(_,_))

  // 3.15
  def concat[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil:List[A])(append)

  // 3.16
  def mapPlusOne(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((x,acc) => Cons(x+1,acc))

  // 3.17
  def mapToString(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((x,acc) => Cons(x.toString,acc))

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((x,acc) => Cons(f(x), acc))

  // 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil:List[A])((x,acc) => if (f(x)) Cons(x,acc) else acc)

  // 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  // 3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(i => if (f(i)) Cons(i,Nil) else Nil)

  // 3.22
  def zipAdd(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(lh,lt), Cons(rh,rt)) => Cons(lh+rh, zipAdd(lt,rt))
  }

  // 3.23
  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = (l,r) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(lh,lt), Cons(rh,rt)) => Cons(f(lh,rh), zipWith(lt,rt)(f))
  }

  // 3.24
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case ls if sub == top(ls,len(sub)) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
  // 3.24 helper -- wow this is inefficient!
  def top[A](l: List[A], n: Int): List[A] = reverse(drop(reverse(l), len(l) - n))

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
object Tree {
  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r)
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => depth(l) + depth(r)
  }

  // 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g:(B,B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  def size2[A](t: Tree[A]): Int = fold(t)(a => 1)(_ + _)
  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
  def depth2[A](t: Tree[A]): Int = fold(t)(a => 1)((x,y) => 1 + (x max y))
}

object Ch3 extends App {
  {
    import com.nothoo.ch3.List._

    // 3.2
    assert(List(1, 2, 3) == tail(List(0, 1, 2, 3)))
    assert(Nil == tail(Nil))
    assert(Nil == tail(List(1)))

    // 3.3
    assert(List(1, 2, 3) == setHead(List(2, 2, 3), 1))
    assert(List(1) == setHead(Nil, 1))

    // 3.4
    assert(List(3) == drop(List(1, 2, 3), 2))

    // 3.5
    assert(List(4, 5) == dropWhile(List(1, 2, 3, 4, 5), (a: Int) => a < 4))

    // 3.6
    assert(List(1, 2, 3) == init(List(1, 2, 3, 4)))

    // 3.8
    assert(List(1, 2, 3) == foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

    // 3.9
    assert(3 == foldRight(List(1, 2, 3), 0)((_, x) => x + 1))

    // 3.10
    assert(3 == foldLeft(List(1, 2, 3), 0)((x, _) => x + 1))

    // 3.11
    assert(6 == sum(List(1, 2, 3)))
    assert(24 == product(List(1, 2, 3, 4)))
    assert(3 == len(List(1, 2, 3)))

    // 3.12
    assert(List(3, 2, 1) == reverseF(List(1, 2, 3)))

    // 3.13
    val f = (acc: Int, x: Int) => acc + 1
    assert(foldLeft(List(1, 2, 3), 0)(f) == foldLeft2(List(1, 2, 3), 0)(f))
    assert(foldRight(List(1, 2, 3), 0)(f) == foldRight2(List(1, 2, 3), 0)(f))

    // 3.14
    assert(List(1, 2, 3) == append(List(1, 2), List(3)))

    // 3.15
    assert(List(1, 2, 3, 4, 5) == concat(List(List(1, 2), List(3, 4), List(5))))

    // 3.16
    assert(List(2, 3, 4) == mapPlusOne(List(1, 2, 3)))

    // 3.17
    assert(List("1.0", "1.1") == mapToString(List(1.0, 1.1)))

    // 3.18
    assert(List(1, 2, 3) == map(List(1, 2, 3))(identity))
    assert(List(1, 2, 3) == map(List(0, 1, 2))(_ + 1))

    // 3.19
    assert(List(2, 4) == filter(List(1, 2, 3, 4))(_ % 2 == 0))

    // 3.20
    assert(List(1, 1, 2, 2, 3, 3) == flatMap(List(1, 2, 3))(i => List(i, i)))

    // 3.21
    assert(List(2, 4) == filter2(List(1, 2, 3, 4))(_ % 2 == 0))

    // 3.22
    assert(List(5, 7, 9) == zipAdd(List(1, 2, 3), List(4, 5, 6)))

    // 3.23
    assert(List("ab", "cd") == zipWith(List("a", "c"), List("b", "d"))(_ + _))

    // 3.24
    assert(List(1, 2) == top(List(1, 2, 3), 2))
    assert(hasSubsequence(List(1, 2, 3), List(1, 2)))
    assert(hasSubsequence(List(1, 2, 3), List(2, 3)))
    assert(hasSubsequence(List(1, 2, 3), List(1)))
    assert(hasSubsequence(List(1, 2, 3), List(1, 2, 3)))
    assert(!hasSubsequence(List(1, 2, 3), List(1, 3)))
  }

  {
    import com.nothoo.ch3.Tree._

    // 3.25
    assert(2 == size(Branch(Leaf(1), Leaf(2))))
    assert(1 == size(Leaf(1)))

    // 3.26
    assert(2 == maximum(Branch(Leaf(1), Leaf(2))))

    // 3.27
    assert(2 == depth(Branch(Leaf(1), Leaf(2))))
    assert(3 == depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))

    // 3.28
    val f = (i: Int) => i * i
    assert(Branch(Leaf(1), Leaf(4)) == map(Branch(Leaf(1), Leaf(2)))(f))
    assert(Branch(Leaf(1), Branch(Leaf(4), Leaf(9))) == map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(f))

    // 3.29
    assert(2 == size2(Branch(Leaf(1), Leaf(2))))
    assert(1 == size2(Leaf(1)))
    assert(2 == maximum2(Branch(Leaf(1), Leaf(2))))
    assert(2 == depth2(Branch(Leaf(1), Leaf(2))))
    assert(3 == depth2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
    assert(1 == depth2(Leaf(1)))
  }
}
