package com.nothoo.ch6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

class Machine(locked: Boolean, candies: Int, coins: Int) {

}

object Machine {
  import State._

  def simulate(inputs: List[Input]): State[Machine, (Int,Int)] = inputs map { i =>
    foo(i,
  }

  def foo(i: Input, s: State[Machine,(Int,Int)]): State[Machine,(Int,Int)] = input match {
    case Coin => modify(s => (i,s))
    case Turn =>
  }
}
