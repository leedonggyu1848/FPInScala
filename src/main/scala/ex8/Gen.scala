package ex8

import ex6.*


object Gen:
  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))
    
  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))
    
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  //8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(State(RNG.double))
      .flatMap(d => if d < g1._2 / (g1._2 + g2._2) then g1._1 else g2._1)

case class Gen[+A](sample: State[RNG, A]):
  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => Gen.listOfN(n, this))