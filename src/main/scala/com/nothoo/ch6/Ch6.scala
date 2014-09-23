package com.nothoo.ch6

object Ch6 extends App {
  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,r) = rng.nextInt
    (if (i<0) -(i+1) else i ,r)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i,r) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,r1) = rng.nextInt
    val (d,r2) = double(r1)
    ((i,d), r2)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case ((i,d),r) => ((d,i),r)
  }
  def double3(rng: RNG): ((Double,Double,Double),RNG) = {
    // omg this sucks
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List(), rng)
    case n =>
      val (i,r) = rng.nextInt
      val (is, r2) = ints(n-1)(r)
      (i :: is, r2)
  }

  type Rand[+A] = RNG => (A,RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a,rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a),r)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // 6.5
  val double2: Rand[Double] = map(_.nextInt)(_ / (Int.MaxValue + 1))

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (a,r1) = ra(rng)
    val (b,r2) = rb(r1)
    (f(a,b), r2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_,_))

  val randIntDouble = both(int, double2)
  val randDoubleInt = both(double2, int)

  // 6.7 -  this one is hard - some form of fold seems the correct route
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val z = unit(List[A]())
    fs.foldRight(z){ (a,as) => map2(a,as)(_ :: _) }
  }

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a,r1) = f(rng)
    g(a)(r1)
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(int){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  // 6.9
  def mapFM[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def map2FM[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))
}

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, newRNG)
  }
}

// 6.10
case class State[S,+A](run: S => (A,S)) {
  import com.nothoo.ch6.State._
  def map[B](f: A => B): State[S,B] = flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] = flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => {
    val (a,s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def unit[S,A](a: A): State[S,A] = State(s => (a,s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}